package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.wallet.SyncHeightDescriptor
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet.{AnyHDWalletApi, CoinSelectionAlgo}
import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateAll}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{GolombFilter, SimpleFilterMatcher}
import org.bitcoins.core.hd._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.{BitcoinScriptUtil, FutureUtil, HDUtil}
import org.bitcoins.core.wallet.builder._
import org.bitcoins.core.wallet.fee._
import org.bitcoins.core.wallet.keymanagement.{
  KeyManagerParams,
  KeyManagerUnlockError
}
import org.bitcoins.core.wallet.utxo.TxoState.{
  ConfirmedReceived,
  PendingConfirmationsReceived
}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.keymanager.bip39.{BIP39KeyManager, BIP39LockedKeyManager}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.internal._
import org.bitcoins.wallet.models._
import scodec.bits.ByteVector

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

abstract class Wallet
    extends AnyHDWalletApi
    with UtxoHandling
    with AddressHandling
    with AccountHandling
    with FundTransactionHandling
    with TransactionProcessing
    with RescanHandling
    with WalletLogger {

  override def keyManager: BIP39KeyManager

  implicit val ec: ExecutionContext

  implicit val walletConfig: WalletAppConfig

  val chainParams: ChainParams = walletConfig.chain

  val networkParameters: NetworkParameters = walletConfig.network

  override val discoveryBatchSize: Int = walletConfig.discoveryBatchSize

  private[wallet] val addressDAO: AddressDAO = AddressDAO()
  private[wallet] val accountDAO: AccountDAO = AccountDAO()
  private[wallet] val spendingInfoDAO: SpendingInfoDAO = SpendingInfoDAO()
  private[wallet] val transactionDAO: TransactionDAO = TransactionDAO()
  private[wallet] val scriptPubKeyDAO: ScriptPubKeyDAO = ScriptPubKeyDAO()

  private[wallet] val incomingTxDAO: IncomingTransactionDAO =
    IncomingTransactionDAO()

  private[wallet] val outgoingTxDAO: OutgoingTransactionDAO =
    OutgoingTransactionDAO()
  private[wallet] val addressTagDAO: AddressTagDAO = AddressTagDAO()

  private[wallet] val stateDescriptorDAO: WalletStateDescriptorDAO =
    WalletStateDescriptorDAO()

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi
  val creationTime: Instant = keyManager.creationTime

  def walletCallbacks: WalletCallbacks = walletConfig.walletCallbacks

  private def utxosWithMissingTx: Future[Vector[SpendingInfoDb]] = {
    for {
      utxos <- spendingInfoDAO.findAllSpendingInfos()
      hasTxs <- FutureUtil.foldLeftAsync(Vector.empty[SpendingInfoDb], utxos) {
        (accum, utxo) =>
          // If we don't have tx in our transactionDAO, add it to the list
          transactionDAO
            .read(utxo.txid)
            .map(txOpt => if (txOpt.isEmpty) accum :+ utxo else accum)
      }
    } yield hasTxs
  }

  protected def downloadMissingUtxos: Future[Unit] =
    for {
      utxos <- utxosWithMissingTx
      blockHashes = utxos.flatMap(_.blockHash.map(_.flip))
      // Download the block the tx is from so we process the block and subsequent txs
      _ <-
        if (blockHashes.nonEmpty) {
          logger.info(
            s"Missing relevant ${utxos.size} wallet transactions, fetching their blocks..")
          nodeApi.downloadBlocks(blockHashes.distinct)
        } else FutureUtil.unit
    } yield ()

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
              s"It is possible we have a different key manager being used than expected, key manager=$keyManager"
          Future.failed(new RuntimeException(errorMsg))
        } else {
          FutureUtil.unit
        }
      case None =>
        val errorMsg = s"Missing root xpub for account $account in database"
        Future.failed(new RuntimeException(errorMsg))
    }
  }

  override def start(): Future[Wallet] = {
    for {
      _ <- walletConfig.start()
      _ <- checkRootAccount
      _ <- downloadMissingUtxos
    } yield {
      startWalletThread()
      this
    }
  }

  override def stop(): Future[Wallet] = {
    for {
      _ <- walletConfig.stop()
    } yield {
      stopWalletThread()
      this
    }
  }

  def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] = {
    stateDescriptorDAO.getSyncDescriptorOpt()
  }

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)]): Future[
    Wallet] = {
    for {
      utxos <- listUtxos()
      scripts <- listScriptPubKeys()
      scriptPubKeys =
        utxos.flatMap(_.redeemScriptOpt).toSet ++ scripts.map(_.scriptPubKey)
      _ <- FutureUtil.sequentially(blockFilters) {
        case (blockHash, blockFilter) =>
          val matcher = SimpleFilterMatcher(blockFilter)
          if (matcher.matchesAny(scriptPubKeys.toVector.map(_.asmBytes))) {
            nodeApi.downloadBlocks(Vector(blockHash))
          } else FutureUtil.unit
      }
      hash = blockFilters.last._1.flip
      height <- chainQueryApi.getBlockHeight(hash)
      _ <- stateDescriptorDAO.updateSyncHeight(hash, height.get)
    } yield {
      this
    }
  }

  def unlock(
      passphraseOpt: Option[AesPassword],
      bip39PasswordOpt: Option[String]): Either[
    KeyManagerUnlockError,
    Wallet] = {
    val kmParams = walletConfig.kmParams

    val unlockedKeyManagerE =
      BIP39LockedKeyManager.unlock(passphraseOpt = passphraseOpt,
                                   bip39PasswordOpt = bip39PasswordOpt,
                                   kmParams = kmParams)
    unlockedKeyManagerE match {
      case Right(km) =>
        val w = Wallet(keyManager = km,
                       nodeApi = nodeApi,
                       chainQueryApi = chainQueryApi,
                       feeRateApi = feeRateApi,
                       creationTime = km.creationTime)
        Right(w)
      case Left(err) => Left(err)
    }
  }

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    for {
      _ <- nodeApi.broadcastTransaction(transaction)
      _ <- walletCallbacks.executeOnTransactionBroadcast(logger, transaction)
    } yield ()

  override def isEmpty(): Future[Boolean] =
    for {
      addressCount <- addressDAO.count()
      spendingInfoCount <- spendingInfoDAO.count()
    } yield addressCount == 0 && spendingInfoCount == 0

  override def clearUtxosAndAddresses(account: HDAccount): Future[Wallet] = {
    for {
      accountUtxos <- spendingInfoDAO.findAllForAccount(account)
      deleteUtxoFs = accountUtxos.map(spendingInfoDAO.delete)
      _ <- FutureUtil.collect(deleteUtxoFs)
      accountAddresses <- addressDAO.findAllForAccount(account)
      deleteAddrFs = accountAddresses.map(addressDAO.delete)
      _ <- FutureUtil.collect(deleteAddrFs)
    } yield this
  }

  override def clearAllUtxosAndAddresses(): Future[Wallet] = {
    for {
      _ <- spendingInfoDAO.deleteAll()
      _ <- addressDAO.deleteAll()
      _ <- scriptPubKeyDAO.deleteAll()
    } yield this
  }

  /** Sums up the value of all unspent
    * TXOs in the wallet, filtered by the given predicate
    */
  private def filterThenSum(
      predicate: SpendingInfoDb => Boolean): Future[CurrencyUnit] = {
    for (
      utxos <-
        spendingInfoDAO.findAllUnspentForAccount(walletConfig.defaultAccount)
    )
      yield {
        val filtered = utxos
          .filter(predicate)
          .map { txo =>
            txo.state match {
              case TxoState.PendingConfirmationsReceived |
                  TxoState.ConfirmedReceived =>
                txo.output.value
              case TxoState.Reserved | TxoState.PendingConfirmationsSpent |
                  TxoState.ConfirmedSpent | TxoState.DoesNotExist |
                  TxoState.ImmatureCoinbase =>
                CurrencyUnits.zero
            }
          }

        filtered.fold(0.sats)(_ + _)
      }
  }

  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    filterThenSum(_.state == ConfirmedReceived).map { balance =>
      logger.trace(s"Confirmed balance=${balance.satoshis}")
      balance
    }
  }

  override def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] = {
    for {
      allUnspent <- spendingInfoDAO.findAllUnspent()
    } yield {
      val confirmedUtxos = allUnspent.filter { utxo =>
        HDAccount.isSameAccount(utxo.privKeyPath.path, account) &&
        utxo.state == ConfirmedReceived
      }
      confirmedUtxos.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  override def getConfirmedBalance(tag: AddressTag): Future[CurrencyUnit] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { allUnspent =>
      val confirmed = allUnspent.filter(_.state == ConfirmedReceived)
      confirmed.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    filterThenSum(_.state == PendingConfirmationsReceived).map { balance =>
      logger.trace(s"Unconfirmed balance=${balance.satoshis}")
      balance
    }
  }

  override def getUnconfirmedBalance(
      account: HDAccount): Future[CurrencyUnit] = {
    for {
      allUnspent <- spendingInfoDAO.findAllUnspent()
    } yield {
      val confirmedUtxos = allUnspent.filter { utxo =>
        HDAccount.isSameAccount(utxo.privKeyPath.path, account) &&
        utxo.state == PendingConfirmationsReceived
      }
      confirmedUtxos.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  override def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { allUnspent =>
      val confirmed = allUnspent.filter(_.state == PendingConfirmationsReceived)
      confirmed.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  /** Enumerates all the TX outpoints in the wallet */
  protected[wallet] def listOutpoints(): Future[Vector[TransactionOutPoint]] =
    spendingInfoDAO.findAllOutpoints()

  /** Gets the size of the bloom filter for this wallet */
  private def getBloomFilterSize(
      pubkeys: Seq[ECPublicKey],
      outpoints: Seq[TransactionOutPoint]): Int = {
    // when a public key is inserted into a filter
    // both the pubkey and the hash of the pubkey
    // gets inserted
    pubkeys.length * 2
  } + outpoints.length

  // todo: insert TXIDs? need to track which txids we should
  // ask for, somehow
  // We add all outpoints to the bloom filter as a way
  // of working around the fact that bloom filters
  // was never updated to incorporate SegWit changes.
  // see this mailing list thread for context:
  //   https://www.mail-archive.com/bitcoin-dev@lists.linuxfoundation.org/msg06950.html
  // especially this email from Jim Posen:
  //   https://www.mail-archive.com/bitcoin-dev@lists.linuxfoundation.org/msg06952.html
  override def getBloomFilter(): Future[BloomFilter] = {
    for {
      pubkeys <- listPubkeys()
      outpoints <- listOutpoints()
    } yield {
      val filterSize = getBloomFilterSize(pubkeys, outpoints)

      // todo: Is this the best flag to use?
      val bloomFlag = BloomUpdateAll

      val baseBloom =
        BloomFilter(numElements = filterSize,
                    falsePositiveRate = walletConfig.bloomFalsePositiveRate,
                    flags = bloomFlag)

      val withPubs = pubkeys.foldLeft(baseBloom) { _.insert(_) }
      outpoints.foldLeft(withPubs) { _.insert(_) }
    }
  }

  /** Takes a [[RawTxBuilderWithFinalizer]] for a transaction to be sent, and completes it by:
    * finalizing and signing the transaction, then correctly processing and logging it
    */
  private def finishSend[F <: RawTxFinalizer](
      txBuilder: RawTxBuilderWithFinalizer[F],
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]],
      sentAmount: CurrencyUnit,
      feeRate: FeeUnit,
      newTags: Vector[AddressTag]): Future[Transaction] = {
    for {
      utx <- txBuilder.buildTx()
      signed <- RawTxSigner.sign(utx, utxoInfos, feeRate)
      ourOuts <- findOurOuts(signed)
      creditingAmount = utxoInfos.foldLeft(CurrencyUnits.zero)(_ + _.amount)
      _ <- processOurTransaction(transaction = signed,
                                 feeRate = feeRate,
                                 inputAmount = creditingAmount,
                                 sentAmount = sentAmount,
                                 blockHashOpt = None,
                                 newTags = newTags)
    } yield {
      logger.debug(
        s"Signed transaction=${signed.txIdBE.hex} with outputs=${signed.outputs.length}, inputs=${signed.inputs.length}")

      logger.trace(s"Change output(s) for transaction=${signed.txIdBE.hex}")
      ourOuts.foreach { out =>
        logger.trace(s"    $out")
      }
      signed
    }
  }

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] = {
    require(
      address.networkParameters.isSameNetworkBytes(networkParameters),
      s"Cannot send to address on other network, got ${address.networkParameters}"
    )
    logger.info(s"Sending to $address at feerate $feeRate")
    for {
      utxoDbs <- spendingInfoDAO.findByOutPoints(outPoints)
      diff = utxoDbs.map(_.outPoint).diff(outPoints)
      _ = require(diff.isEmpty,
                  s"Not all OutPoints belong to this wallet, diff $diff")
      spentUtxos =
        utxoDbs.filterNot(utxo => TxoState.receivedStates.contains(utxo.state))
      _ = require(
        spentUtxos.isEmpty,
        s"Some out points given have already been spent, ${spentUtxos.map(_.outPoint)}")

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
      finalizer = SubtractFeeFromOutputsFinalizer(inputInfos,
                                                  feeRate,
                                                  Vector(address.scriptPubKey))
        .andThen(ShuffleFinalizer)
        .andThen(AddWitnessDataFinalizer(inputInfos))

      withFinalizer = txBuilder.setFinalizer(finalizer)

      tmp <- withFinalizer.buildTx()

      _ = require(
        tmp.outputs.size == 1,
        s"Created tx is not as expected, does not have 1 output, got $tmp")

      tx <- finishSend(withFinalizer,
                       utxos,
                       tmp.outputs.head.value,
                       feeRate,
                       Vector.empty)
    } yield tx
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    require(
      address.networkParameters.isSameNetworkBytes(networkParameters),
      s"Cannot send to address on other network, got ${address.networkParameters}"
    )
    logger.info(s"Sending $amount to $address at feerate $feeRate")
    for {
      utxoDbs <- spendingInfoDAO.findByOutPoints(outPoints)
      diff = utxoDbs.map(_.outPoint).diff(outPoints)
      _ = require(diff.isEmpty,
                  s"Not all OutPoints belong to this wallet, diff $diff")
      spentUtxos =
        utxoDbs.filterNot(utxo => TxoState.receivedStates.contains(utxo.state))
      _ = require(
        spentUtxos.isEmpty,
        s"Some out points given have already been spent, ${spentUtxos.map(_.outPoint)}")

      prevTxFs = utxoDbs.map(utxo =>
        transactionDAO.findByOutPoint(utxo.outPoint).map(_.get.transaction))
      prevTxs <- FutureUtil.collect(prevTxFs)
      utxos =
        utxoDbs
          .zip(prevTxs)
          .map(info => info._1.toUTXOInfo(keyManager, info._2))

      changeAddr <- getNewChangeAddress(fromAccount.hdAccount)

      output = TransactionOutput(amount, address.scriptPubKey)
      txBuilder = ShufflingNonInteractiveFinalizer.txBuilderFrom(
        Vector(output),
        utxos,
        feeRate,
        changeAddr.scriptPubKey)

      tx <- finishSend(txBuilder, utxos, amount, feeRate, newTags)
    } yield tx
  }

  override def bumpFeeRBF(
      txId: DoubleSha256DigestBE,
      newFeeRate: FeeUnit): Future[Transaction] = {
    for {
      txDbOpt <- transactionDAO.findByTxId(txId)
      tx <- txDbOpt match {
        case Some(db) => Future.successful(db.transaction)
        case None =>
          Future.failed(
            new RuntimeException(s"Unable to find transaction ${txId.hex}"))
      }

      _ = require(TxUtil.isRBFEnabled(tx), "Transaction is not signaling RBF")

      outPoints = tx.inputs.map(_.previousOutput).toVector
      spks = tx.outputs.map(_.scriptPubKey).toVector

      utxos <- spendingInfoDAO.findByOutPoints(outPoints)
      _ = require(utxos.nonEmpty, "Can only bump fee for our own transaction")
      _ = require(utxos.size == tx.inputs.size,
                  "Can only bump fee for a transaction we own all the inputs")

      oldOutputs <- spendingInfoDAO.findDbsForTx(txId)
      blockHashes = oldOutputs.flatMap(_.blockHash).distinct
      _ = require(
        blockHashes.isEmpty,
        s"Cannot replace a confirmed transaction, ${blockHashes.map(_.hex)}")

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
          s"Cannot bump to a lower fee ${oldFeeRate.currencyUnit} < ${newFeeRate.currencyUnit}")
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

      // Mark old outputs as replaced
      _ <- spendingInfoDAO.updateAll(
        oldOutputs.map(_.copyWithState(TxoState.DoesNotExist)))

      sequence = tx.inputs.head.sequence + UInt32.one
      outputs = tx.outputs.filterNot(_.scriptPubKey == changeSpk)
      txBuilder = StandardNonInteractiveFinalizer.txBuilderFrom(outputs,
                                                                spendingInfos,
                                                                newFeeRate,
                                                                changeSpk,
                                                                sequence)

      amount = outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
      tx <-
        finishSend(txBuilder, spendingInfos, amount, newFeeRate, Vector.empty)
    } yield tx
  }

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    require(
      address.networkParameters.isSameNetworkBytes(networkParameters),
      s"Cannot send to address on other network, got ${address.networkParameters}"
    )
    logger.info(s"Sending $amount to $address at feerate $feeRate")
    val destination = TransactionOutput(amount, address.scriptPubKey)
    for {
      (txBuilder, utxoInfos) <- fundRawTransactionInternal(
        destinations = Vector(destination),
        feeRate = feeRate,
        fromAccount = fromAccount,
        coinSelectionAlgo = algo,
        fromTagOpt = None)

      tx <- finishSend(txBuilder, utxoInfos, amount, feeRate, newTags)
    } yield tx
  }

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendWithAlgo(address,
                 amount,
                 feeRate,
                 CoinSelectionAlgo.AccumulateLargest,
                 fromAccount)

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendWithAlgo(address,
                 amount,
                 feeRate,
                 CoinSelectionAlgo.AccumulateLargest,
                 fromAccount,
                 newTags)

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    require(amounts.size == addresses.size,
            "Must have an amount for every address")
    require(
      addresses.forall(
        _.networkParameters.isSameNetworkBytes(networkParameters)),
      s"Cannot send to address on other network, got ${addresses.map(_.networkParameters)}"
    )
    val destinations = addresses.zip(amounts).map {
      case (address, amount) =>
        logger.info(s"Sending $amount to $address at feerate $feeRate")
        TransactionOutput(amount, address.scriptPubKey)
    }
    sendToOutputs(destinations, feeRate, fromAccount, newTags)
  }

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val messageToUse = if (hashMessage) {
      CryptoUtil.sha256(ByteVector(message.getBytes)).bytes
    } else {
      if (message.length > 80) {
        throw new IllegalArgumentException(
          s"Message cannot be greater than 80 characters, it should be hashed, got $message")
      } else ByteVector(message.getBytes)
    }

    val asm = Seq(OP_RETURN) ++ BitcoinScriptUtil.calculatePushOp(
      messageToUse) :+ ScriptConstant(messageToUse)

    val scriptPubKey = ScriptPubKey(asm)

    val output = TransactionOutput(0.satoshis, scriptPubKey)

    for {
      (txBuilder, utxoInfos) <- fundRawTransactionInternal(
        destinations = Vector(output),
        feeRate = feeRate,
        fromAccount = fromAccount,
        coinSelectionAlgo = CoinSelectionAlgo.RandomSelection,
        fromTagOpt = None)
      tx <- finishSend(txBuilder,
                       utxoInfos,
                       CurrencyUnits.zero,
                       feeRate,
                       Vector.empty)
    } yield tx
  }

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      (txBuilder, utxoInfos) <- fundRawTransactionInternal(
        destinations = outputs,
        feeRate = feeRate,
        fromAccount = fromAccount,
        fromTagOpt = None)
      sentAmount = outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
      tx <- finishSend(txBuilder, utxoInfos, sentAmount, feeRate, newTags)
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
      feeRate: FeeUnit): Future[Transaction] = {
    for {
      txDbOpt <- transactionDAO.findByTxId(txId)
      tx <- txDbOpt match {
        case Some(db) => Future.successful(db.transaction)
        case None =>
          Future.failed(
            new RuntimeException(s"Unable to find transaction ${txId.hex}"))
      }

      spendingInfos <- spendingInfoDAO.findTx(tx)
      _ = require(spendingInfos.nonEmpty,
                  s"Transaction ${txId.hex} must have an output we own")

      oldOutputs <- spendingInfoDAO.findDbsForTx(txId)
      blockHashes = oldOutputs.flatMap(_.blockHash).distinct
      _ = require(
        blockHashes.isEmpty,
        s"No need to fee bump a confirmed transaction, ${blockHashes.map(_.hex)}")

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

      addr <- getNewChangeAddress()
      childTx <- sendFromOutPoints(Vector(spendingInfo.outPoint), addr, feeRate)
    } yield childTx
  }

  override def signPSBT(psbt: PSBT)(implicit
      ec: ExecutionContext): Future[PSBT] = {
    val inputTxIds = psbt.transaction.inputs.zipWithIndex.map {
      case (input, index) =>
        input.previousOutput.txIdBE -> index
    }.toMap
    for {
      accountDbs <- accountDAO.findAll()
      ourXpubs = accountDbs.map(_.xpub)
      utxos <- spendingInfoDAO.findAll()
      txs <- transactionDAO.findByTxIds(inputTxIds.keys.toVector)

      updated = txs.foldLeft(psbt) { (accum, tx) =>
        val index = inputTxIds(tx.txIdBE)
        accum.addUTXOToInput(tx.transaction, index)
      }

      signed <-
        FutureUtil.foldLeftAsync(updated, updated.inputMaps.zipWithIndex) {
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
                      unsigned.addRedeemOrWitnessScriptToInput(redeemScript,
                                                               index)
                    case None => unsigned
                  }

                  (Vector(utxo.path), psbtWithUtxoData)
                case None => (Vector.empty, unsigned)
              }
            }

            val keyPaths = xpubKeyPaths ++ utxoPath

            FutureUtil.foldLeftAsync(withData, keyPaths) { (accum, hdPath) =>
              val sign = keyManager.toSign(hdPath)
              // Only sign if that key doesn't have a signature yet
              if (!input.partialSignatures.exists(_.pubKey == sign.publicKey)) {
                logger.debug(
                  s"Signing input $index with key ${sign.publicKey.hex}")
                accum.sign(index, sign)
              } else {
                Future.successful(accum)
              }
            }
        }
    } yield {
      if (updated == signed) {
        logger.warn("Did not find any keys or utxos that belong to this wallet")
      }
      signed
    }
  }

  protected def getLastAccountOpt(
      purpose: HDPurpose): Future[Option[AccountDb]] = {
    accountDAO
      .findAll()
      .map(_.filter(_.hdAccount.purpose == purpose))
      .map(_.sortBy(_.hdAccount.index))
      // we want to the most recently created account,
      // to know what the index of our new account
      // should be.
      .map(_.lastOption)
  }

  /** Creates a new account my reading from our account database, finding the last account,
    * and then incrementing the account index by one, and then creating that account
    *
    * @return
    */
  override def createNewAccount(kmParams: KeyManagerParams): Future[Wallet] = {
    getLastAccountOpt(kmParams.purpose).flatMap {
      case Some(accountDb) =>
        val hdAccount = accountDb.hdAccount
        val newAccount = hdAccount.copy(index = hdAccount.index + 1)
        createNewAccount(newAccount, kmParams)
      case None =>
        createNewAccount(walletConfig.defaultAccount, kmParams)
    }
  }

  // todo: check if there's addresses in the most recent
  // account before creating new
  override def createNewAccount(
      hdAccount: HDAccount,
      kmParams: KeyManagerParams): Future[Wallet] = {
    logger.info(
      s"Creating new account at index ${hdAccount.index} for purpose ${kmParams.purpose}")

    val xpub: ExtPublicKey = {
      keyManager.deriveXPub(hdAccount) match {
        case Failure(exception) =>
          // this won't happen, because we're deriving from a privkey
          // this should really be changed in the method signature
          logger.error(s"Unexpected error when deriving xpub: $exception")
          throw exception
        case Success(xpub) => xpub
      }
    }
    val newAccountDb = AccountDb(xpub, hdAccount)
    accountDAO.create(newAccountDb).map { created =>
      logger.debug(s"Created new account ${created.hdAccount}")
      this
    }
  }
}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends WalletLogger {

  private case class WalletImpl(
      override val keyManager: BIP39KeyManager,
      override val nodeApi: NodeApi,
      override val chainQueryApi: ChainQueryApi,
      override val feeRateApi: FeeRateApi,
      override val creationTime: Instant
  )(implicit
      override val walletConfig: WalletAppConfig,
      override val ec: ExecutionContext
  ) extends Wallet

  def apply(
      keyManager: BIP39KeyManager,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      creationTime: Instant)(implicit
      config: WalletAppConfig,
      ec: ExecutionContext): Wallet = {
    WalletImpl(keyManager, nodeApi, chainQueryApi, feeRateApi, creationTime)
  }

  /** Creates the level 0 account for the given HD purpose, if the root account exists do nothing */
  private def createRootAccount(wallet: Wallet, keyManager: BIP39KeyManager)(
      implicit ec: ExecutionContext): Future[AccountDb] = {
    val coinType = HDUtil.getCoinType(keyManager.kmParams.network)
    val coin =
      HDCoin(purpose = keyManager.kmParams.purpose, coinType = coinType)
    val account = HDAccount(coin = coin, index = 0)
    // safe since we're deriving from a priv
    val xpub = keyManager.deriveXPub(account).get
    val accountDb = AccountDb(xpub, account)

    //see if we already have this account in our database
    //Three possible cases:
    //1. We have nothing in our database, so we need to insert it
    //2. We already have this account in our database, so we do nothing
    //3. We have this account in our database, with a DIFFERENT xpub. This is bad. Fail with an exception
    //   this most likely means that we have a different key manager than we expected
    wallet.accountDAO.read((account.coin, account.index)).flatMap {
      case Some(account) =>
        if (account.xpub != xpub) {
          val errorMsg =
            s"Divergent xpubs for account=${account}. Existing database xpub=${account.xpub}, new xpub=${xpub}. " +
              s"It is possible we have a different key manager being used than expected, keymanager=${keyManager}"
          Future.failed(new RuntimeException(errorMsg))
        } else {
          logger.debug(
            s"Account already exists in database, no need to create it, account=${account}")
          Future.successful(account)
        }
      case None =>
        wallet.accountDAO
          .create(accountDb)
          .map { written =>
            logger.info(s"Created account=${accountDb} to DB")
            written
          }
    }

  }

  def initialize(wallet: Wallet, bip39PasswordOpt: Option[String])(implicit
      walletAppConfig: WalletAppConfig,
      ec: ExecutionContext): Future[Wallet] = {
    val passwordOpt = walletAppConfig.aesPasswordOpt
    // We want to make sure all level 0 accounts are created,
    // so the user can change the default account kind later
    // and still have their wallet work
    def createAccountFutures: Future[Vector[Future[AccountDb]]] =
      for {
        _ <- walletAppConfig.start()
        accounts = HDPurposes.singleSigPurposes.map { purpose =>
          //we need to create key manager params for each purpose
          //and then initialize a key manager to derive the correct xpub
          val kmParams = wallet.keyManager.kmParams.copy(purpose = purpose)
          val kmE = {
            BIP39KeyManager.fromParams(kmParams = kmParams,
                                       passwordOpt = passwordOpt,
                                       bip39PasswordOpt = bip39PasswordOpt)
          }
          kmE match {
            case Right(km) =>
              createRootAccount(wallet = wallet, keyManager = km)
            case Left(err) =>
              //probably means you haven't initialized the key manager via the
              //'CreateKeyManagerApi'
              Future.failed(new RuntimeException(
                s"Failed to create keymanager with params=$kmParams err=$err"))
          }

        }
      } yield accounts

    createAccountFutures.flatMap(accounts => FutureUtil.collect(accounts)).map {
      _ =>
        logger.debug(s"Created root level accounts for wallet")
        wallet
    }
  }
}
