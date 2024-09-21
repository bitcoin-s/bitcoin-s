package org.bitcoins.wallet.internal

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.core.wallet.builder.*
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{SpendingInfoDAO, TransactionDAO}
import org.bitcoins.wallet.WalletLogger
import scodec.bits.ByteVector
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}

import scala.concurrent.Future

case class FundTransactionHandling(
    accountHandling: AccountHandling,
    utxoHandling: UtxoHandling,
    addressHandling: AddressHandlingApi,
    transactionProcessing: TransactionProcessingApi,
    spendingInfoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    keyManager: BIP39KeyManagerApi,
    feeRateApi: FeeRateApi)(implicit
    walletConfig: WalletAppConfig,
    system: ActorSystem)
    extends FundTransactionHandlingApi
    with WalletLogger {
  // import walletConfig.profile.api._
  import org.bitcoins.core.currency.currencyUnitNumeric
  import system.dispatcher
  private val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase

  override def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    for {
      account <- accountHandling.getDefaultAccount()
      funded <- fundRawTransaction(
        destinations = destinations,
        feeRate = feeRate,
        fromAccount = account,
        fromTagOpt = fromTagOpt,
        markAsReserved = markAsReserved
      )
    } yield funded
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      fromTagOpt: Option[AddressTag] = None,
      markAsReserved: Boolean = false
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundRawTransactionInternal(
      destinations = destinations,
      feeRate = feeRate,
      fromAccount = fromAccount,
      fromTagOpt = fromTagOpt,
      markAsReserved = markAsReserved
    )
  }

  /** Funds an unsigned transaction from the specified account */
  override def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      markAsReserved: Boolean
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundRawTransaction(
      destinations = destinations,
      feeRate = feeRate,
      fromAccount = fromAccount,
      fromTagOpt = None,
      markAsReserved = markAsReserved
    )
  }

  /** This returns a [[RawTxBuilder]] that can be used to generate an unsigned
    * transaction with [[RawTxBuilder.result()]] which can be signed with the
    * returned [[ScriptSignatureParams]].
    *
    * Utxos are funded with the given coin selection algorithm
    */
  private[bitcoins] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.LeastWaste,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    val action = fundRawTransactionInternalAction(
      destinations,
      feeRate,
      fromAccount,
      coinSelectionAlgo,
      fromTagOpt,
      markAsReserved
    )

    for {
      txHelper <- safeDatabase.run(action)
      _ <- txHelper.reservedUTXOsCallbackF
    } yield txHelper
  }

  private[bitcoins] def fundRawTransactionInternalAction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.LeastWaste,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean
  ): DBIOAction[FundRawTxHelper[
                  ShufflingNonInteractiveFinalizer
                ],
                NoStream,
                Effect.Read with Effect.Write with Effect.Transactional] = {
    val amts = destinations.map(_.value)
    // need to allow 0 for OP_RETURN outputs
    require(
      amts.forall(_.satoshis.toBigInt >= 0),
      s"Cannot fund a transaction for a negative amount, got=$amts"
    )
    val amt = amts.sum
    logger.info(s"Attempting to fund a tx for amt=$amt with feeRate=$feeRate")
    val utxosA =
      for {
        utxos <- fromTagOpt match {
          case None =>
            spendingInfoDAO.findAllUnspentForAccountAction(
              fromAccount.hdAccount
            )
          case Some(tag) =>
            spendingInfoDAO.findAllUnspentForTagAction(tag).map { utxos =>
              utxos.filter(utxo =>
                HDAccount.isSameAccount(
                  bip32Path = utxo.privKeyPath,
                  account = fromAccount.hdAccount
                ))
            }
        }
        utxoWithTxs <- DBIO.sequence {
          utxos.map { utxo =>
            transactionDAO
              .findByTxIdAction(utxo.outPoint.txIdBE)
              .map(tx => (utxo, tx.get.transaction))
          }
        }

        // Need to remove immature coinbase inputs
        immatureCoinbases = utxoWithTxs.filter(
          _._1.state == TxoState.ImmatureCoinbase
        )
      } yield utxoWithTxs.filter(utxo =>
        !immatureCoinbases.exists(_._1 == utxo._1))

    val selectedUtxosA =
      for {
        walletUtxos <- utxosA

        // filter out dust
        selectableUtxos = walletUtxos
          .map(_._1)
          .filter(_.output.value > Policy.dustThreshold)
          .map(CoinSelectorUtxo.fromSpendingInfoDb)

        utxos = CoinSelector.selectByAlgo(
          coinSelectionAlgo = coinSelectionAlgo,
          walletUtxos = selectableUtxos,
          outputs = destinations,
          feeRate = feeRate,
          longTermFeeRateOpt = Some(walletConfig.longTermFeeRate)
        )
        filtered = walletUtxos.filter(utxo =>
          utxos.exists(_.outPoint == utxo._1.outPoint))
        (_, callbackF) <-
          if (markAsReserved)
            utxoHandling.markUTXOsAsReservedAction(filtered.map(_._1))
          else DBIO.successful((Vector.empty, Future.unit))
      } yield (filtered, callbackF)

    for {
      (selectedUtxos, callbackF) <- selectedUtxosA
      change <- accountHandling.getNewChangeAddressAction(fromAccount)
      utxoSpendingInfos = {
        selectedUtxos.map { case (utxo, prevTx) =>
          utxo.toUTXOInfo(keyManager = keyManager, prevTx)
        }
      }
    } yield {
      val utxosStr = selectedUtxos.map { utxo =>
        s"${utxo._1.outPoint} state=${utxo._1.state}"
      }
      logger.info(s"Spending UTXOs: $utxosStr")

      utxoSpendingInfos.zipWithIndex.foreach { case (utxo, index) =>
        logger.info(s"UTXO $index details: ${utxo.output}")
      }

      val txBuilder =
        ShufflingNonInteractiveFinalizer.txBuilderFrom(
          destinations,
          utxoSpendingInfos,
          feeRate,
          change.scriptPubKey
        )

      val fundTxHelper = FundRawTxHelper(
        txBuilderWithFinalizer = txBuilder,
        scriptSigParams = utxoSpendingInfos,
        feeRate = feeRate,
        reservedUTXOsCallbackF = callbackF
      )

      fundTxHelper
    }
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
      fundRawTxHelper <- fundRawTransactionInternal(
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
        feeRate,
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

  private def determineFeeRate(feeRateOpt: Option[FeeUnit]): Future[FeeUnit] =
    feeRateOpt match {
      case None =>
        feeRateApi.getFeeRate()
      case Some(feeRate) =>
        Future.successful(feeRate)
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
