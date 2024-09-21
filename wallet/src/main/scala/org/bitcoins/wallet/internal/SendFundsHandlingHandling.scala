package org.bitcoins.wallet.internal

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.api.wallet.{
  AccountHandlingApi,
  AddressHandlingApi,
  CoinSelectionAlgo,
  SendFundsHandlingApi,
  TransactionProcessingApi,
  UtxoHandlingApi
}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{
  InputUtil,
  Transaction,
  TransactionOutPoint,
  TransactionOutput,
  TxUtil
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.builder.{
  AddWitnessDataFinalizer,
  FundRawTxHelper,
  RawTxBuilder,
  RawTxFinalizer,
  ShuffleFinalizer,
  ShufflingNonInteractiveFinalizer,
  StandardNonInteractiveFinalizer,
  SubtractFeeFromOutputsFinalizer
}
import org.bitcoins.core.wallet.fee.{
  FeeUnit,
  SatoshisPerByte,
  SatoshisPerKW,
  SatoshisPerKiloByte,
  SatoshisPerVirtualByte
}
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  SpendingInfoDAO,
  TransactionDAO,
  WalletDAOs
}

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
  import org.bitcoins.core.currency.currencyUnitNumeric
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
      tx <- finishSend(rawTxHelper, amount, feeRate, newTags)
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
      tx <- finishSend(fundRawTxHelper, sentAmount, feeRate, newTags)
    } yield tx
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit
  ): Future[Transaction] = {
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
}
