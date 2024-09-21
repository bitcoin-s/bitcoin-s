package org.bitcoins.wallet

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db.{
  AccountDb,
  SpendingInfoDb,
  TransactionDb
}
import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.*
import org.bitcoins.core.gcs.{GolombFilter, SimpleFilterMatcher}
import org.bitcoins.core.hd.*
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.{FutureUtil, HDUtil}
import org.bitcoins.core.wallet.builder.*
import org.bitcoins.core.wallet.fee.*
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.core.wallet.utxo.TxoState.*
import org.bitcoins.crypto.*
import org.bitcoins.db.SafeDatabase
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.internal.*
import org.bitcoins.wallet.models.*
import slick.dbio.{DBIOAction, Effect, NoStream}

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.Random

abstract class Wallet extends NeutrinoHDWalletApi with WalletLogger {

  override def keyManager: BIP39KeyManager = {
    walletConfig.kmConf.toBip39KeyManager
  }
  implicit val walletConfig: WalletAppConfig

  implicit val system: ActorSystem = walletConfig.system

  implicit val ec: ExecutionContext = system.dispatcher

  private[wallet] lazy val scheduler = walletConfig.scheduler

  val chainParams: ChainParams = walletConfig.chain

  val networkParameters: BitcoinNetwork = walletConfig.network

  private[bitcoins] val addressDAO: AddressDAO = AddressDAO()
  private[bitcoins] val accountDAO: AccountDAO = AccountDAO()
  private[bitcoins] val spendingInfoDAO: SpendingInfoDAO = SpendingInfoDAO()
  private[bitcoins] val transactionDAO: TransactionDAO = TransactionDAO()
  private[bitcoins] val scriptPubKeyDAO: ScriptPubKeyDAO = ScriptPubKeyDAO()

  private[bitcoins] val incomingTxDAO: IncomingTransactionDAO =
    IncomingTransactionDAO()

  private[bitcoins] val outgoingTxDAO: OutgoingTransactionDAO =
    OutgoingTransactionDAO()
  private[bitcoins] val addressTagDAO: AddressTagDAO = AddressTagDAO()

  private[bitcoins] val stateDescriptorDAO: WalletStateDescriptorDAO =
    WalletStateDescriptorDAO()

  private def walletDAOs: WalletDAOs = WalletDAOs(accountDAO,
                                                  addressDAO,
                                                  addressTagDAO,
                                                  spendingInfoDAO,
                                                  transactionDAO,
                                                  incomingTxDAO,
                                                  outgoingTxDAO,
                                                  scriptPubKeyDAO,
                                                  stateDescriptorDAO)

  protected lazy val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi
  val creationTime: Instant = keyManager.creationTime

  def utxoHandling: UtxoHandling =
    UtxoHandling(spendingInfoDAO, transactionDAO, addressDAO, chainQueryApi)

  def fundTxHandling: FundTransactionHandling = FundTransactionHandling(
    accountHandling = accountHandling,
    utxoHandling = utxoHandling,
    addressHandling = addressHandling,
    transactionProcessing = transactionProcessing,
    spendingInfoDAO = spendingInfoDAO,
    transactionDAO = transactionDAO,
    keyManager = keyManager,
    feeRateApi = feeRateApi
  )
  def accountHandling: AccountHandling =
    AccountHandling(walletDAOs, keyManager)

  def addressHandling: AddressHandling =
    AddressHandling(accountHandling, walletDAOs)

  override lazy val transactionProcessing: TransactionProcessingApi = {
    TransactionProcessing(
      walletApi = this,
      chainQueryApi = chainQueryApi,
      utxoHandling = utxoHandling,
      walletDAOs = walletDAOs
    )
  }
  override lazy val rescanHandling: RescanHandlingApi = {
    RescanHandling(
      transactionProcessing = transactionProcessing,
      accountHandling = accountHandling,
      addressHandling = addressHandling,
      chainQueryApi = chainQueryApi,
      nodeApi = nodeApi,
      walletDAOs = walletDAOs
    )
  }

  override def isRescanning(): Future[Boolean] = rescanHandling.isRescanning()

  def walletCallbacks: WalletCallbacks = walletConfig.callBacks

  private def checkRootAccount: Future[Unit] = {
    val coinType = HDUtil.getCoinType(keyManager.kmParams.network)
    val coin =
      HDCoin(purpose = keyManager.kmParams.purpose, coinType = coinType)
    val account = HDAccount(coin = coin, index = 0)
    // safe since we're deriving from a priv
    val xpub = keyManager.deriveXPub(account).get

    accountDAO.read((account.coin, account.index)).flatMap {
      case Some(account) =>
        if (account.xpub != xpub) {
          val errorMsg =
            s"Divergent xpubs for account=$account. Existing database xpub=${account.xpub}, key manager's xpub=$xpub. " +
              s"It is possible we have a different key manager being used than expected, key manager=${keyManager.kmParams.seedPath.toAbsolutePath.toString}"
          Future.failed(new RuntimeException(errorMsg))
        } else {
          Future.unit
        }
      case None =>
        val errorMsg = s"Missing root xpub for account $account in database"
        Future.failed(new RuntimeException(errorMsg))
    }
  }

  override def start(): Future[Wallet] = {
    logger.info("Starting Wallet")

    checkRootAccount.map { _ =>
      walletConfig.startRebroadcastTxsScheduler(this)
      startFeeRateCallbackScheduler()
      this
    }
  }

  override def stop(): Future[Wallet] = {
    Future.successful(this)
  }

  override def getNewAddress(): Future[BitcoinAddress] = {
    addressHandling.getNewAddress()
  }

  override def getNewChangeAddress(): Future[BitcoinAddress] = {
    addressHandling.getNewChangeAddress()
  }

  override def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] = {
    stateDescriptorDAO.getSyncHeight()
  }

  override def getSyncState(): Future[BlockSyncState] = {
    getSyncDescriptorOpt().map {
      case Some(descriptor) =>
        BlockSyncState(descriptor.height, descriptor.bestHash)
      case None =>
        BlockSyncState(0, chainParams.genesisHashBE)
    }
  }

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  ): Future[Wallet] = {
    val utxosF = utxoHandling.listUtxos()
    val spksF = addressHandling.listScriptPubKeys()
    val blockHashOpt = blockFilters.lastOption.map(_._1)
    val heightOptF = blockHashOpt match {
      case Some(blockHash) =>
        chainQueryApi.getBlockHeight(blockHash)
      case None => Future.successful(None)
    }
    for {
      utxos <- utxosF
      scripts <- spksF
      _ = logger.debug(
        s"Processing ${blockFilters.length} block filters for ${utxos.length} utxos and ${scripts.length} scripts"
      )
      scriptPubKeys =
        utxos.flatMap(_.redeemScriptOpt).toSet ++ scripts.map(_.scriptPubKey)
      blockHashToDownload <- {
        if (scriptPubKeys.isEmpty) {
          // do nothing as an optimization, if we have nothing in the wallet
          // we don't need to search the filters
          Future.successful(Vector.empty)
        } else {
          FutureUtil
            .batchAndParallelExecute(
              elements = blockFilters,
              f = searchFilterMatches(scriptPubKeys.toVector)
            )
            .map(_.flatten)
        }
      }
      _ <- nodeApi.downloadBlocks(blockHashToDownload)
      hash = blockFilters.last._1
      heightOpt <- heightOptF
      _ <- {
        heightOpt match {
          case Some(height) =>
            if (blockHashToDownload.isEmpty) {
              // if we don't have any block hashes
              // we need to update the wallet's sync height
              stateDescriptorDAO
                .updateSyncHeight(hash, height)
                .map(_ => ())
            } else {
              // if we do have a block hash that we matched
              // we need to let wallet.processBlock()
              // update the wallet's sync height
              Future.unit
            }
          case None =>
            Future.unit
        }
      }
    } yield {
      this
    }
  }

  private def searchFilterMatches(spks: Vector[ScriptPubKey])(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  ): Future[Vector[DoubleSha256DigestBE]] = FutureUtil.makeAsync { () =>
    val asmVec = spks.map(_.asmBytes)
    blockFilters.flatMap { case (blockHash, blockFilter) =>
      val matcher = SimpleFilterMatcher(blockFilter)
      if (matcher.matchesAny(asmVec)) {
        Vector(blockHash)
      } else {
        Vector.empty
      }
    }
  }

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    for {
      _ <- nodeApi.broadcastTransaction(transaction)
      _ <- transactionProcessing.processTransaction(transaction,
                                                    blockHashOpt = None)
      _ <- walletCallbacks.executeOnTransactionBroadcast(transaction)
    } yield ()

  override def getTransactionsToBroadcast: Future[Vector[Transaction]] = {
    for {
      mempoolUtxos <- spendingInfoDAO.findAllInMempool
      txIds = mempoolUtxos.map { utxo =>
        utxo.spendingTxIdOpt.getOrElse(utxo.txid)
      }
      txDbs <- transactionDAO.findByTxIdBEs(txIds)
    } yield txDbs.map(_.transaction)
  }

  override def isEmpty(): Future[Boolean] =
    for {
      addressCount <- addressDAO.count()
      spendingInfoCount <- spendingInfoDAO.count()
    } yield addressCount == 0 && spendingInfoCount == 0

  override def getBalance()(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = {
    safeDatabase.run(spendingInfoDAO.getBalanceAction())
  }

  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    safeDatabase.run(spendingInfoDAO.getConfirmedBalanceAction())
  }
  override def getConfirmedBalance(tag: AddressTag): Future[CurrencyUnit] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { allUnspent =>
      val confirmed = allUnspent.filter(_.state == ConfirmedReceived)
      confirmed.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    safeDatabase.run(spendingInfoDAO.getUnconfirmedBalanceAction())
  }

  override def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { allUnspent =>
      val confirmed = allUnspent
        .filter(utxo => TxoState.pendingReceivedStates.contains(utxo.state))
      confirmed.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  override def findByOutPoints(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByOutPoints(outPoints)
  }

  override def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]
  ): Future[Vector[TransactionDb]] = {
    transactionDAO.findByTxIds(txIds)
  }

  override def findOutputsBeingSpent(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findOutputsBeingSpent(tx)
  }

  /** Takes a [[RawTxBuilderWithFinalizer]] for a transaction to be sent, and
    * completes it by: finalizing and signing the transaction, then correctly
    * processing and logging it
    */
  private def finishSend[F <: RawTxFinalizer](
      rawTxHelper: FundRawTxHelper[F],
      sentAmount: CurrencyUnit,
      feeRate: FeeUnit,
      newTags: Vector[AddressTag]
  ): Future[Transaction] = {
    val signed = rawTxHelper.signedTx

    val processedTxF = for {
      ourOuts <- addressHandling.findOurOutputs(signed)
      creditingAmount = rawTxHelper.scriptSigParams.foldLeft(
        CurrencyUnits.zero
      )(_ + _.amount)
      _ <- transactionProcessing.processOurTransaction(
        transaction = signed,
        feeRate = feeRate,
        inputAmount = creditingAmount,
        sentAmount = sentAmount,
        blockHashOpt = None,
        newTags = newTags
      )
    } yield {
      logger.debug(
        s"Signed transaction=${signed.txIdBE.hex} with outputs=${signed.outputs.length}, inputs=${signed.inputs.length}"
      )

      logger.trace(s"Change output(s) for transaction=${signed.txIdBE.hex}")
      ourOuts.foreach { out =>
        logger.trace(s"    $out")
      }
      signed
    }

    processedTxF.recoverWith { case _ =>
      // if something fails, we need to unreserve the utxos associated with this tx
      // and then propogate the failed future upwards
      utxoHandling.unmarkUTXOsAsReserved(signed).flatMap(_ => processedTxF)
    }
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    require(
      address.networkParameters.isSameNetworkBytes(networkParameters),
      s"Cannot send to address on other network, got ${address.networkParameters}"
    )
    logger.info(s"Sending to $address at feerate $feeRate")
    for {
      utxoDbs <- spendingInfoDAO.findByOutPoints(outPoints)
      diff = utxoDbs.map(_.outPoint).diff(outPoints)
      _ = require(
        diff.isEmpty,
        s"Not all OutPoints belong to this wallet, diff $diff"
      )
      spentUtxos =
        utxoDbs.filterNot(utxo => TxoState.receivedStates.contains(utxo.state))
      _ = require(
        spentUtxos.isEmpty,
        s"Some out points given have already been spent, ${spentUtxos.map(_.outPoint)}"
      )

      utxos <- Future.sequence {
        utxoDbs.map(utxo =>
          transactionDAO
            .findByOutPoint(utxo.outPoint)
            .map(txDb => utxo.toUTXOInfo(keyManager, txDb.get.transaction)))
      }
      inputInfos = utxos.map(_.inputInfo)

      utxoAmount = utxoDbs.map(_.output.value).sum
      dummyOutput = TransactionOutput(utxoAmount, address.scriptPubKey)
      inputs = InputUtil.calcSequenceForInputs(utxos)

      txBuilder = RawTxBuilder() ++= inputs += dummyOutput
      finalizer = SubtractFeeFromOutputsFinalizer(
        inputInfos,
        feeRate,
        Vector(address.scriptPubKey)
      )
        .andThen(ShuffleFinalizer)
        .andThen(AddWitnessDataFinalizer(inputInfos))

      withFinalizer = txBuilder.setFinalizer(finalizer)

      tmp = withFinalizer.buildTx()

      _ = require(
        tmp.outputs.size == 1,
        s"Created tx is not as expected, does not have 1 output, got $tmp"
      )
      rawTxHelper = FundRawTxHelper(withFinalizer, utxos, feeRate, Future.unit)
      tx <- finishSend(
        rawTxHelper,
        tmp.outputs.head.value,
        feeRate,
        Vector.empty
      )
    } yield tx
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    require(
      address.networkParameters.isSameNetworkBytes(networkParameters),
      s"Cannot send to address on other network, got ${address.networkParameters}"
    )
    logger.info(s"Sending $amount to $address at feerate $feeRate")
    for {
      utxoDbs <- spendingInfoDAO.findByOutPoints(outPoints)
      diff = utxoDbs.map(_.outPoint).diff(outPoints)
      _ = require(
        diff.isEmpty,
        s"Not all OutPoints belong to this wallet, diff $diff"
      )
      spentUtxos =
        utxoDbs.filterNot(utxo => TxoState.receivedStates.contains(utxo.state))
      _ = require(
        spentUtxos.isEmpty,
        s"Some out points given have already been spent, ${spentUtxos.map(_.outPoint)}"
      )

      prevTxFs = utxoDbs.map(utxo =>
        transactionDAO.findByOutPoint(utxo.outPoint).map(_.get.transaction))
      prevTxs <- FutureUtil.collect(prevTxFs)
      utxos =
        utxoDbs
          .zip(prevTxs)
          .map(info => info._1.toUTXOInfo(keyManager, info._2))

      changeAddr <- accountHandling.getNewChangeAddress(fromAccount.hdAccount)

      output = TransactionOutput(amount, address.scriptPubKey)
      txBuilder = ShufflingNonInteractiveFinalizer.txBuilderFrom(
        Vector(output),
        utxos,
        feeRate,
        changeAddr.scriptPubKey
      )
      rawTxHelper = FundRawTxHelper(txBuilder, utxos, feeRate, Future.unit)
      tx <- finishSend(rawTxHelper, amount, feeRate, newTags)
    } yield tx
  }

  /** Sends the entire wallet balance to the given address */
  override def sweepWallet(address: BitcoinAddress, feeRate: FeeUnit)(implicit
      ec: ExecutionContext
  ): Future[Transaction] = {
    for {
      utxos <- utxoHandling.listUtxos()
      balance = utxos.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
      _ = logger.info(s"Sweeping wallet balance=$balance to address=$address")
      outpoints = utxos.map(_.outPoint)
      tx <- sendFromOutPoints(outpoints, address, feeRate)
    } yield tx
  }

  override def bumpFeeRBF(
      txId: DoubleSha256DigestBE,
      newFeeRate: FeeUnit
  ): Future[Transaction] = {
    for {
      txDbOpt <- transactionDAO.findByTxId(txId)
      txDb <- txDbOpt match {
        case Some(db) => Future.successful(db)
        case None =>
          Future.failed(
            new RuntimeException(s"Unable to find transaction ${txId.hex}")
          )
      }
      tx = txDb.transaction

      _ = require(TxUtil.isRBFEnabled(tx), "Transaction is not signaling RBF")

      outPoints = tx.inputs.map(_.previousOutput).toVector
      spks = tx.outputs.map(_.scriptPubKey).toVector

      utxos <- spendingInfoDAO.findByOutPoints(outPoints)
      _ = require(utxos.nonEmpty, "Can only bump fee for our own transaction")
      _ = require(
        utxos.size == tx.inputs.size,
        "Can only bump fee for a transaction we own all the inputs"
      )

      _ = require(
        txDb.blockHashOpt.isEmpty,
        s"Cannot replace a confirmed transaction, ${txDb.blockHashOpt.get.hex}"
      )

      spendingInfos <- FutureUtil.sequentially(utxos) { utxo =>
        transactionDAO
          .findByOutPoint(utxo.outPoint)
          .map(txDbOpt =>
            utxo.toUTXOInfo(keyManager = keyManager, txDbOpt.get.transaction))
      }

      _ = {
        val inputAmount = utxos.foldLeft(CurrencyUnits.zero)(_ + _.output.value)

        val oldFeeRate = newFeeRate match {
          case _: SatoshisPerByte =>
            SatoshisPerByte.calc(inputAmount, tx)
          case _: SatoshisPerKiloByte =>
            SatoshisPerKiloByte.calc(inputAmount, tx)
          case _: SatoshisPerVirtualByte =>
            SatoshisPerVirtualByte.calc(inputAmount, tx)
          case _: SatoshisPerKW =>
            SatoshisPerKW.calc(inputAmount, tx)
        }

        require(
          oldFeeRate.currencyUnit < newFeeRate.currencyUnit,
          s"Cannot bump to a lower fee ${oldFeeRate.currencyUnit} < ${newFeeRate.currencyUnit}"
        )
      }

      myAddrs <- addressDAO.findByScriptPubKeys(spks)
      _ = require(myAddrs.nonEmpty, "Must have an output we own")

      changeSpks = myAddrs.flatMap { db =>
        if (db.isChange) {
          Some(db.scriptPubKey)
        } else None
      }

      changeSpk =
        if (changeSpks.nonEmpty) {
          // Pick a random change spk
          Random.shuffle(changeSpks).head
        } else {
          // If none are explicit change, pick a random one we own
          Random.shuffle(myAddrs.map(_.scriptPubKey)).head
        }

      oldOutputs <- spendingInfoDAO.findDbsForTx(txId)
      // delete old outputs
      _ <- spendingInfoDAO.deleteAll(oldOutputs)

      sequence = tx.inputs.head.sequence + UInt32.one
      outputs = tx.outputs.filterNot(_.scriptPubKey == changeSpk)
      txBuilder = StandardNonInteractiveFinalizer.txBuilderFrom(
        outputs,
        spendingInfos,
        newFeeRate,
        changeSpk,
        sequence
      )

      amount = outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
      rawTxHelper = FundRawTxHelper(
        txBuilder,
        spendingInfos,
        newFeeRate,
        Future.unit
      )
      tx <-
        finishSend(rawTxHelper, amount, newFeeRate, Vector.empty)
    } yield tx
  }

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    require(
      address.networkParameters.isSameNetworkBytes(networkParameters),
      s"Cannot send to address on other network, got ${address.networkParameters}"
    )
    logger.info(s"Sending $amount to $address at feerate $feeRate")
    val destination = TransactionOutput(amount, address.scriptPubKey)
    for {
      rawTxHelper <- fundTxHandling.fundRawTransactionInternal(
        destinations = Vector(destination),
        feeRate = feeRate,
        fromAccount = fromAccount,
        coinSelectionAlgo = algo,
        fromTagOpt = None,
        markAsReserved = true
      )
      tx <- finishSend(rawTxHelper, amount, feeRate, newTags)
    } yield tx
  }

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    sendWithAlgo(
      address,
      amount,
      feeRate,
      CoinSelectionAlgo.LeastWaste,
      fromAccount
    )

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] =
    sendWithAlgo(
      address,
      amount,
      feeRate,
      CoinSelectionAlgo.LeastWaste,
      fromAccount,
      newTags
    )

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    require(
      amounts.size == addresses.size,
      "Must have an amount for every address"
    )
    require(
      addresses.forall(
        _.networkParameters.isSameNetworkBytes(networkParameters)
      ),
      s"Cannot send to address on other network, got ${addresses.map(_.networkParameters)}"
    )
    val destinations = addresses.zip(amounts).map { case (address, amount) =>
      logger.info(s"Sending $amount to $address at feerate $feeRate")
      TransactionOutput(amount, address.scriptPubKey)
    }
    sendToOutputs(destinations, feeRate, fromAccount, newTags)
  }

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      fundRawTxHelper <- fundTxHandling.fundRawTransactionInternal(
        destinations = outputs,
        feeRate = feeRate,
        fromAccount = fromAccount,
        fromTagOpt = None,
        markAsReserved = true
      )
      sentAmount = outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
      tx <- finishSend(fundRawTxHelper, sentAmount, feeRate, newTags)
    } yield tx
  }

  /** @inheritdoc */
  override def isChange(output: TransactionOutput): Future[Boolean] = {
    addressDAO.findByScriptPubKey(output.scriptPubKey).map {
      case Some(db) => db.isChange
      case None     => false
    }
  }

  /** @inheritdoc */
  override def bumpFeeCPFP(
      txId: DoubleSha256DigestBE,
      feeRate: FeeUnit
  ): Future[Transaction] = {
    for {
      txDbOpt <- transactionDAO.findByTxId(txId)
      txDb <- txDbOpt match {
        case Some(db) => Future.successful(db)
        case None =>
          Future.failed(
            new RuntimeException(s"Unable to find transaction ${txId.hex}")
          )
      }
      tx = txDb.transaction

      spendingInfos <- spendingInfoDAO.findTx(tx)
      _ = require(
        spendingInfos.nonEmpty,
        s"Transaction ${txId.hex} must have an output we own"
      )

      _ = require(
        txDb.blockHashOpt.isEmpty,
        s"Cannot replace a confirmed transaction, ${txDb.blockHashOpt.get.hex}"
      )

      changeSpendingInfos = spendingInfos.flatMap { db =>
        if (db.isChange) {
          Some(db)
        } else None
      }

      spendingInfo =
        if (changeSpendingInfos.nonEmpty) {
          // Pick a random change spendingInfo
          Random.shuffle(changeSpendingInfos).head
        } else {
          // If none are explicit change, pick a random one we own
          Random.shuffle(spendingInfos).head
        }

      addr <- addressHandling.getNewChangeAddress()
      childTx <- sendFromOutPoints(Vector(spendingInfo.outPoint), addr, feeRate)
    } yield childTx
  }

  override def signPSBT(
      psbt: PSBT
  )(implicit ec: ExecutionContext): Future[PSBT] = {
    val inputTxIds = psbt.transaction.inputs.zipWithIndex.map {
      case (input, index) =>
        input.previousOutput.txIdBE -> index
    }.toMap
    for {
      accountDbs <- accountDAO.findAll()
      ourXpubs = accountDbs.map(_.xpub)
      utxos <- spendingInfoDAO.findAll()
      txs <- transactionDAO.findByTxIds(inputTxIds.keys.toVector)
    } yield {
      val updated = txs.foldLeft(psbt) { (accum, tx) =>
        val index = inputTxIds(tx.txIdBE)
        accum.addUTXOToInput(tx.transaction, index)
      }

      val signed =
        updated.inputMaps.zipWithIndex.foldLeft(updated) {
          case (unsigned, (input, index)) =>
            val xpubKeyPaths = input.BIP32DerivationPaths
              .filter { path =>
                ourXpubs.exists(_.fingerprint == path.masterFingerprint)
              }
              .map(bip32Path =>
                HDPath.fromString(
                  bip32Path.path.toString
                )) // TODO add a way to get a HDPath from a BIP32 Path

            val (utxoPath, withData) = {
              val outPoint = unsigned.transaction.inputs(index).previousOutput
              utxos.find(_.outpoint == outPoint) match {
                case Some(utxo) =>
                  val psbtWithUtxoData = utxo.redeemScript match {
                    case Some(redeemScript) =>
                      unsigned.addRedeemOrWitnessScriptToInput(
                        redeemScript,
                        index
                      )
                    case None => unsigned
                  }

                  (Vector(utxo.path), psbtWithUtxoData)
                case None => (Vector.empty, unsigned)
              }
            }

            val keyPaths = xpubKeyPaths ++ utxoPath

            keyPaths.foldLeft(withData) { (accum, hdPath) =>
              val sign = keyManager.toSign(hdPath)
              // Only sign if that key doesn't have a signature yet
              if (
                !input.partialSignatures.exists(
                  _.pubKey.toPublicKey == sign.publicKey
                )
              ) {
                logger.debug(
                  s"Signing input $index with key ${sign.publicKey.hex}"
                )
                accum.sign(index, sign)
              } else {
                accum
              }
            }
        }

      if (updated == signed) {
        logger.warn("Did not find any keys or utxos that belong to this wallet")
      }
      signed
    }
  }

  override def getWalletName(): Future[String] = {
    Future.successful(walletConfig.walletName)
  }

  override def getInfo(): Future[WalletInfo] = {
    for {
      accountDb <- accountHandling.getDefaultAccount()
      walletState <- getSyncState()
      rescan <- rescanHandling.isRescanning()
    } yield {
      WalletInfo(
        walletName = walletConfig.walletName,
        rootXpub = keyManager.getRootXPub,
        xpub = accountDb.xpub,
        hdAccount = accountDb.hdAccount,
        height = walletState.height,
        blockHash = walletState.blockHash,
        rescan = rescan,
        imported = keyManager.imported
      )
    }
  }

  override def findByScriptPubKey(
      scriptPubKey: ScriptPubKey
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByScriptPubKey(scriptPubKey)
  }

  def startFeeRateCallbackScheduler(): Unit = {
    val feeRateChangedRunnable = new Runnable {
      override def run(): Unit = {
        getFeeRate()
          .map(feeRate => Some(feeRate))
          .recover { case NonFatal(_) =>
            // logger.error("Cannot get fee rate ", ex)
            None
          }
          .foreach { feeRateOpt =>
            walletCallbacks.executeOnFeeRateChanged(
              feeRateOpt.getOrElse(SatoshisPerVirtualByte.negativeOne)
            )
          }
        ()
      }
    }

    val _ = scheduler.scheduleAtFixedRate(
      feeRateChangedRunnable,
      walletConfig.feeRatePollDelay.toSeconds,
      walletConfig.feeRatePollInterval.toSeconds,
      TimeUnit.SECONDS
    )
  }

  override def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]] =
    utxoHandling.updateUtxoPendingStates()

  override def listTransactions(): Future[Vector[TransactionDb]] =
    transactionProcessing.listTransactions()

  override def listUtxos(): Future[Vector[SpendingInfoDb]] =
    utxoHandling.listUtxos()

  override def listUtxos(state: TxoState): Future[Vector[SpendingInfoDb]] =
    utxoHandling.listUtxos(state)

  override def listUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    utxoHandling.listUtxos(tag)
  }

  def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    utxoHandling.markUTXOsAsReserved(utxos)
  }

  /** Marks all utxos that are ours in this transactions as reserved */
  override def markUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    utxoHandling.markUTXOsAsReserved(tx)
  }

  override def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    utxoHandling.unmarkUTXOsAsReserved(utxos)
  }

  /** Unmarks all utxos that are ours in this transactions indicating they are
    * no longer reserved
    */
  override def unmarkUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    utxoHandling.unmarkUTXOsAsReserved(tx)
  }
}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends WalletLogger {

  private case class WalletImpl(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi
  )(implicit
      val walletConfig: WalletAppConfig
  ) extends Wallet

  def apply(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi
  )(implicit config: WalletAppConfig): Wallet = {
    WalletImpl(nodeApi, chainQueryApi, feeRateApi)
  }

  /** Creates the master xpub for the key manager in the database
    * @throws RuntimeException
    *   if a different master xpub key exists in the database
    */
  private def createMasterXPub(
      keyManager: BIP39KeyManager
  )(implicit walletAppConfig: WalletAppConfig): Future[ExtPublicKey] = {
    import walletAppConfig.ec
    val masterXPubDAO = MasterXPubDAO()
    val countF = masterXPubDAO.count()
    // make sure we don't have a xpub in the db
    countF.flatMap { count =>
      if (count == 0) {
        masterXPubDAO.create(keyManager.getRootXPub).map(_.toExtPublicKey)
      } else {
        for {
          xpubs <- masterXPubDAO.findAll()
        } yield {
          if (
            xpubs.length == 1 && xpubs.head.toExtPublicKey == keyManager.getRootXPub
          ) {
            xpubs.head.toExtPublicKey
          } else {
            throw new IllegalArgumentException(
              s"Wallet database contains different master xpubs, got=${xpubs}"
            )
          }
        }
      }
    }

  }

  /** Creates the level 0 account for the given HD purpose, if the root account
    * exists do nothing
    */
  private def createRootAccount(wallet: Wallet, keyManager: BIP39KeyManager)(
      implicit ec: ExecutionContext
  ): DBIOAction[AccountDb, NoStream, Effect.Read with Effect.Write] = {
    val coinType = HDUtil.getCoinType(keyManager.kmParams.network)
    val coin =
      HDCoin(purpose = keyManager.kmParams.purpose, coinType = coinType)
    val account = HDAccount(coin = coin, index = 0)
    // safe since we're deriving from a priv
    val xpub = keyManager.deriveXPub(account).get
    val accountDb = AccountDb(xpub, account)

    // see if we already have this account in our database
    // Three possible cases:
    // 1. We have nothing in our database, so we need to insert it
    // 2. We already have this account in our database, so we do nothing
    // 3. We have this account in our database, with a DIFFERENT xpub. This is bad. Fail with an exception
    //   this most likely means that we have a different key manager than we expected
    wallet.accountDAO
      .findByPrimaryKeyAction((account.coin, account.index))
      .flatMap {
        case Some(account) =>
          if (account.xpub != xpub) {
            val errorMsg =
              s"Divergent xpubs for account=${account}. Existing database xpub=${account.xpub}, new xpub=${xpub}. " +
                s"It is possible we have a different key manager being used than expected, keymanager=${keyManager.kmParams.seedPath.toAbsolutePath.toString}"
            DBIOAction.failed(new RuntimeException(errorMsg))
          } else {
            logger.debug(
              s"Account already exists in database, no need to create it, account=${account}"
            )
            DBIOAction.successful(account)
          }
        case None =>
          wallet.accountDAO
            .createAction(accountDb)
      }
  }

  def initialize(
      wallet: Wallet,
      accountHandling: AccountHandlingApi,
      bip39PasswordOpt: Option[String]
  ): Future[Wallet] = {
    implicit val walletAppConfig: WalletAppConfig = wallet.walletConfig
    import walletAppConfig.ec
    val passwordOpt = walletAppConfig.aesPasswordOpt

    val createMasterXpubF = createMasterXPub(wallet.keyManager)
    // We want to make sure all level 0 accounts are created,
    // so the user can change the default account kind later
    // and still have their wallet work
    val createAccountActions: Vector[
      DBIOAction[AccountDb, NoStream, Effect.Read with Effect.Write]
    ] = {
      val accounts = HDPurpose.singleSigPurposes.map { purpose =>
        // we need to create key manager params for each purpose
        // and then initialize a key manager to derive the correct xpub
        val kmParams = wallet.keyManager.kmParams.copy(purpose = purpose)
        val kmE = {
          BIP39KeyManager.fromParams(
            kmParams = kmParams,
            passwordOpt = passwordOpt,
            bip39PasswordOpt = bip39PasswordOpt
          )
        }
        kmE match {
          case Right(km) =>
            createRootAccount(wallet = wallet, keyManager = km)
          case Left(err) =>
            // probably means you haven't initialized the key manager via the
            // 'CreateKeyManagerApi'
            DBIOAction.failed(
              new RuntimeException(
                s"Failed to create keymanager with params=$kmParams err=$err"
              )
            )
        }

      }
      accounts
    }
    for {
      _ <- createMasterXpubF
      actions = createAccountActions
      accounts <- wallet.accountDAO.safeDatabase.runVec(
        DBIOAction.sequence(actions)
      )
      _ = accounts.foreach { a =>
        logger.info(s"Created account=${a} to DB")
      }
      _ <- {
        // check if creationTime is well in the past, if so generate a pool of addresses
        // see: https://github.com/bitcoin-s/bitcoin-s/issues/5033
        val creationTime = wallet.keyManager.creationTime
        val threshold = Instant.now().minus(1, ChronoUnit.HOURS)
        val isOldCreationTime = creationTime.compareTo(threshold) <= 0
        if (isOldCreationTime) {
          accountHandling
            .generateScriptPubKeys(
              account = walletAppConfig.defaultAccount,
              addressBatchSize = walletAppConfig.discoveryBatchSize,
              forceGenerateSpks = true
            )
            .map(_ => ())
        } else {
          // fresh seed, no need to generate addresses
          Future.unit
        }
      }
    } yield {
      logger.debug(s"Created root level accounts for wallet")
      wallet
    }
  }
}
