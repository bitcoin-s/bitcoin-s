package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet.{CoinSelectionAlgo, CoinSelector}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.builder._
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
      .map(_._1.buildTx())
  }

  /** This returns a [[RawTxBuilder]] that can be used to generate an unsigned transaction with [[RawTxBuilder.result()]]
    * which can be signed with the returned [[ScriptSignatureParams]].
    *
    * Utxos are funded with the given coin selection algorithm
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
        immatureCoinbases = utxoWithTxs.filter(
          _._1.state == TxoState.ImmatureCoinbase)
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
        selectedUtxos.map { case (utxo, prevTx) =>
          utxo.toUTXOInfo(keyManager = self.keyManager, prevTx)
        }
      }
      _ <-
        if (markAsReserved) markUTXOsAsReserved(selectedUtxos.map(_._1))
        else Future.unit
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

      utxoSpendingInfos.zipWithIndex.foreach { case (utxo, index) =>
        logger.info(s"UTXO $index details: ${utxo.output}")
      }

      val txBuilder = ShufflingNonInteractiveFinalizer.txBuilderFrom(
        destinations,
        utxoSpendingInfos,
        feeRate,
        change.scriptPubKey)

      (txBuilder, utxoSpendingInfos)
    }

    resultF.recoverWith { case NonFatal(error) =>
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
