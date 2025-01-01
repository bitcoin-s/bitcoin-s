package org.bitcoins.wallet.internal

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet.{
  AccountHandlingApi,
  AddressHandlingApi,
  CoinSelectionAlgo,
  SendFundsHandlingApi,
  TransactionProcessingApi,
  UtxoHandlingApi
}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput,
  TxUtil
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.{BitcoinScriptUtil, FutureUtil}
import org.bitcoins.core.wallet.builder.{
  FundRawTxHelper,
  RawTxFinalizer,
  ShufflingNonInteractiveFinalizer,
  StandardNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.{
  FeeUnit,
  SatoshisPerByte,
  SatoshisPerKW,
  SatoshisPerKiloByte,
  SatoshisPerVirtualByte
}
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.{CryptoUtil, DigitalSignature, DoubleSha256DigestBE}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  SpendingInfoDAO,
  TransactionDAO,
  WalletDAOs
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

case class SendFundsHandlingHandling(
    accountHandling: AccountHandlingApi,
    feeRateApi: FeeRateApi,
    fundTxHandling: FundTransactionHandling,
    addressHandling: AddressHandlingApi,
    transactionProcessing: TransactionProcessingApi,
    utxoHandling: UtxoHandlingApi,
    keyManager: BIP39KeyManagerApi,
    walletDAOs: WalletDAOs)(implicit walletConfig: WalletAppConfig)
    extends SendFundsHandlingApi
    with BitcoinSLogger {
  import walletConfig.ec
  private val networkParameters: NetworkParameters = walletConfig.network
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private val transactionDAO: TransactionDAO = walletDAOs.transactionDAO
  private val accountDAO: AccountDAO = walletDAOs.accountDAO

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
      map = SpendingInfoDb.toPreviousOutputMap(utxos)
      spendingInfos <- FutureUtil.sequentially(utxos) { utxo =>
        transactionDAO
          .findByOutPoint(utxo.outPoint)
          .map(txDbOpt =>
            utxo.toUTXOInfo(keyManager = keyManager,
                            txDbOpt.get.transaction,
                            previousOutputMap = map))
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
        finishSend(rawTxHelper, amount, Vector.empty)
    } yield tx
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

  override def getTransactionsToBroadcast: Future[Vector[Transaction]] = {
    for {
      mempoolUtxos <- spendingInfoDAO.findAllInMempool
      txIds = mempoolUtxos.map { utxo =>
        utxo.spendingTxIdOpt.getOrElse(utxo.txid)
      }
      txDbs <- transactionDAO.findByTxIdBEs(txIds)
    } yield txDbs.map(_.transaction)
  }

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit,
      fromAccount: AccountDb
  ): Future[Transaction] = {
    val messageToUse = if (hashMessage) {
      CryptoUtil.sha256(ByteVector(message.getBytes)).bytes
    } else {
      if (message.length > 80) {
        throw new IllegalArgumentException(
          s"Message cannot be greater than 80 characters, it should be hashed, got $message"
        )
      } else ByteVector(message.getBytes)
    }

    val asm = Seq(OP_RETURN) ++ BitcoinScriptUtil.calculatePushOp(
      messageToUse
    ) :+ ScriptConstant(messageToUse)

    val scriptPubKey = ScriptPubKey(asm)

    val output = TransactionOutput(Satoshis.zero, scriptPubKey)

    for {
      fundRawTxHelper <- fundTxHandling.fundRawTransactionInternal(
        destinations = Vector(output),
        feeRate = feeRate,
        fromAccount = fromAccount,
        coinSelectionAlgo = CoinSelectionAlgo.RandomSelection,
        fromTagOpt = None,
        markAsReserved = true
      )
      tx <- finishSend(
        fundRawTxHelper,
        CurrencyUnits.zero,
        Vector.empty
      )
    } yield tx
  }

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate, fromAccount)
    } yield tx
  }

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit): Future[Transaction] = {
    for {
      account <- accountHandling.getDefaultAccount()
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate, account)
    } yield tx
  }

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit]): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate)
    } yield tx
  }

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  ): Future[Transaction] = {
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
      tx <- finishSend(rawTxHelper, amount, newTags)
    } yield tx
  }

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  ): Future[Transaction] = {
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

      map = SpendingInfoDb.toPreviousOutputMap(utxoDbs)
      utxosWithPrevTx <- fundTxHandling.getPreviousTransactions(utxoDbs)
      utxos = utxosWithPrevTx.map(u =>
        u._1.toUTXOInfo(keyManager, u._2.transaction, map))
      changeAddr <- accountHandling.getNewChangeAddress(fromAccount.hdAccount)

      output = TransactionOutput(amount, address.scriptPubKey)
      txBuilder = ShufflingNonInteractiveFinalizer.txBuilderFrom(
        Vector(output),
        utxos,
        feeRate,
        changeAddr.scriptPubKey
      )
      rawTxHelper = FundRawTxHelper(txBuilder, utxos, feeRate, Future.unit)
      tx <- finishSend(rawTxHelper, amount, newTags)
    } yield tx
  }

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  ): Future[Transaction] = {
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

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  ): Future[Transaction] = {
    for {
      fundRawTxHelper <- fundTxHandling.fundRawTransactionInternal(
        destinations = outputs,
        feeRate = feeRate,
        fromAccount = fromAccount,
        fromTagOpt = None,
        markAsReserved = true
      )
      sentAmount = outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
      tx <- finishSend(fundRawTxHelper, sentAmount, newTags)
    } yield tx
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit
  ): Future[Transaction] = {
    val invariantF = Future(
      require(outPoints.nonEmpty, s"Cannot sendFromOutpoints with 0 outpoints"))
    val outputs = Vector(TransactionOutput(Satoshis.zero, address.scriptPubKey))
    for {
      _ <- invariantF
      utxos <- utxoHandling.getUtxos(outPoints)
      outputMap = SpendingInfoDb.toPreviousOutputMap(utxos)
      utxosWithTxs <- fundTxHandling.getPreviousTransactions(utxos)
      inputInfos = utxosWithTxs
        .map(u =>
          u._1.toUTXOInfo(keyManager,
                          prevTransaction = u._2.transaction,
                          previousOutputMap = outputMap))
        .map(_.inputInfo)
      dummyTx = TxUtil.buildDummyTx(inputInfos, outputs)
      fee = feeRate.calc(dummyTx)
      totalBalance = utxos.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
      amountMinusFee = totalBalance - fee
      tx <- sendFromOutPoints(outPoints = outPoints,
                              address = address,
                              amount = amountMinusFee,
                              feeRate = feeRate,
                              newTags = Vector.empty)
    } yield tx
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
              val sigExists = input
                .partialSignatures[DigitalSignature]
                .exists(
                  _.pubKey.toPublicKey == sign.publicKey
                )
              if (!sigExists) {
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

  /** Takes a [[RawTxBuilderWithFinalizer]] for a transaction to be sent, and
    * completes it by: finalizing and signing the transaction, then correctly
    * processing and logging it
    */
  private def finishSend[F <: RawTxFinalizer](
      rawTxHelper: FundRawTxHelper[F],
      sentAmount: CurrencyUnit,
      newTags: Vector[AddressTag]
  ): Future[Transaction] = {
    val signed = rawTxHelper.signedTx
    val feeRate = rawTxHelper.feeRate
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
        blockHashWithConfsOpt = None,
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

  private def determineFeeRate(feeRateOpt: Option[FeeUnit]): Future[FeeUnit] =
    feeRateOpt match {
      case None =>
        feeRateApi.getFeeRate()
      case Some(feeRate) =>
        Future.successful(feeRate)
    }
}
