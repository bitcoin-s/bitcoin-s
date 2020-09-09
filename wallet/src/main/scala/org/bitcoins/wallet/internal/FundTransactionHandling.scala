package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet.{CoinSelectionAlgo, CoinSelector}
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.builder.{
  RawTxBuilder,
  RawTxBuilderWithFinalizer,
  ShufflingNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.Future
import scala.util.control.NonFatal

trait FundTransactionHandling extends WalletLogger { self: Wallet =>

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      funded <- fundRawTransaction(destinations = destinations,
                                   feeRate = feeRate,
                                   fromAccount = account,
                                   fromTagOpt = fromTagOpt,
                                   markAsReserved = markAsReserved)
    } yield funded
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      fromTagOpt: Option[AddressTag] = None,
      markAsReserved: Boolean = false): Future[Transaction] = {
    fundRawTransactionInternal(destinations = destinations,
                               feeRate = feeRate,
                               fromAccount = fromAccount,
                               fromTagOpt = fromTagOpt,
                               markAsReserved = markAsReserved)
      .flatMap(_._1.buildTx())
  }

  /** This returns a [[RawTxBuilder]] that can be used to generate an unsigned transaction with [[RawTxBuilder.result()]]
    * which can be used with signing.
    *
    * If you pass in a [[KeyManagerApi]], the [[org.bitcoins.core.wallet.utxo.ScriptSignatureParams.signers signers]]
    * will be populated with valid signers that can be used to produce valid [[org.bitcoins.crypto.ECDigitalSignature signatures]]
    *
    * If you do not pass in a key manager, the transaction built by [[RawTxBuilder txbuilder]] will contain [[org.bitcoins.core.protocol.script.EmptyScriptSignature EmptyScriptSignature]]
    *
    * Currently utxos are funded with [[CoinSelector.accumulateLargest() accumulateLargest]] coin seleciton algorithm
    */
  private[wallet] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo =
        CoinSelectionAlgo.AccumulateLargest,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean = false): Future[(
      RawTxBuilderWithFinalizer[ShufflingNonInteractiveFinalizer],
      Vector[ScriptSignatureParams[InputInfo]])] = {
    def utxosF: Future[Vector[(SpendingInfoDb, Transaction)]] =
      for {
        utxos <- fromTagOpt match {
          case None =>
            listUtxos(fromAccount.hdAccount)
          case Some(tag) =>
            listUtxos(fromAccount.hdAccount, tag)
        }

        utxoWithTxs <- FutureUtil.sequentially(utxos) { utxo =>
          transactionDAO
            .findByOutPoint(utxo.outPoint)
            .map(tx => (utxo, tx.get.transaction))
        }

        // Need to remove immature coinbase inputs
        coinbaseUtxos = utxoWithTxs.filter(_._2.isCoinbase)
        confFs = coinbaseUtxos.map(utxo =>
          chainQueryApi
            .getNumberOfConfirmations(utxo._1.blockHash.get)
            .map((utxo, _)))
        confs <- FutureUtil.collect(confFs)
        immatureCoinbases =
          confs
            .filter {
              case (_, confsOpt) =>
                confsOpt.isDefined && confsOpt.get < Consensus.coinbaseMaturity
            }
            .map(_._1)
      } yield utxoWithTxs.filter(utxo =>
        !immatureCoinbases.exists(_._1 == utxo._1))

    val selectedUtxosF: Future[Vector[(SpendingInfoDb, Transaction)]] =
      for {
        walletUtxos <- utxosF
        utxos = CoinSelector.selectByAlgo(coinSelectionAlgo = coinSelectionAlgo,
                                          walletUtxos = walletUtxos.map(_._1),
                                          outputs = destinations,
                                          feeRate = feeRate)

      } yield walletUtxos.filter(utxo => utxos.contains(utxo._1))

    val resultF = for {
      selectedUtxos <- selectedUtxosF
      change <- getNewChangeAddress(fromAccount)
      utxoSpendingInfos = {
        selectedUtxos.map {
          case (utxo, prevTx) =>
            utxo.toUTXOInfo(keyManager = self.keyManager, prevTx)
        }
      }
      _ <-
        if (markAsReserved) markUTXOsAsReserved(selectedUtxos.map(_._1))
        else FutureUtil.unit
    } yield {
      logger.info {
        val utxosStr = utxoSpendingInfos
          .map { utxo =>
            import utxo.outPoint
            s"${outPoint.txId.hex}:${outPoint.vout.toInt}"
          }
          .mkString(", ")
        s"Spending UTXOs: $utxosStr"
      }

      utxoSpendingInfos.zipWithIndex.foreach {
        case (utxo, index) =>
          logger.info(s"UTXO $index details: ${utxo.output}")
      }

      val inputs =
        InputUtil.calcSequenceForInputs(utxoSpendingInfos)

      val lockTime = TxUtil.calcLockTime(utxoSpendingInfos).get

      val txBuilder =
        RawTxBuilder().setLockTime(lockTime) ++= destinations ++= inputs

      val finalizer = ShufflingNonInteractiveFinalizer(
        utxoSpendingInfos.map(_.inputInfo),
        feeRate,
        change.scriptPubKey)

      (txBuilder.setFinalizer(finalizer), utxoSpendingInfos)
    }

    resultF.recoverWith {
      case NonFatal(error) =>
        // un-reserve utxos since we failed to create valid spending infos
        if (markAsReserved) {
          for {
            utxos <- selectedUtxosF
            _ <- unmarkUTXOsAsReserved(utxos.map(_._1))
          } yield error
        } else Future.failed(error)
    }

    resultF
  }
}
