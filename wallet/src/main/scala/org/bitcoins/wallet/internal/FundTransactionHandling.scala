package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet.{CoinSelectionAlgo, CoinSelector}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction._
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
  private[bitcoins] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.LeastWaste,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean): Future[(
      RawTxBuilderWithFinalizer[ShufflingNonInteractiveFinalizer],
      Vector[ScriptSignatureParams[InputInfo]])] = {
    val amt = destinations.map(_.value).sum
    logger.info(s"Attempting to fund a tx for amt=${amt} with feeRate=$feeRate")
    val utxosF: Future[Vector[(SpendingInfoDb, Transaction)]] =
      for {
        utxos <- fromTagOpt match {
          case None =>
            listUtxos(fromAccount.hdAccount)
          case Some(tag) =>
            listUtxos(fromAccount.hdAccount, tag)
        }
        utxoWithTxs <- Future.sequence {
          utxos.map { utxo =>
            transactionDAO
              .findByOutPoint(utxo.outPoint)
              .map(tx => (utxo, tx.get.transaction))
          }
        }

        // Need to remove immature coinbase inputs
        immatureCoinbases = utxoWithTxs.filter(
          _._1.state == TxoState.ImmatureCoinbase)
      } yield utxoWithTxs.filter(utxo =>
        !immatureCoinbases.exists(_._1 == utxo._1))

    val selectedUtxosF: Future[Vector[(SpendingInfoDb, Transaction)]] =
      for {
        walletUtxos <- utxosF

        // filter out dust
        selectableUtxos = walletUtxos
          .map(_._1)
          .filter(_.output.value > Policy.dustThreshold)

        utxos = CoinSelector.selectByAlgo(
          coinSelectionAlgo = coinSelectionAlgo,
          walletUtxos = selectableUtxos,
          outputs = destinations,
          feeRate = feeRate,
          longTermFeeRateOpt = Some(self.walletConfig.longTermFeeRate)
        )
        filtered = walletUtxos.filter(utxo => utxos.contains(utxo._1))
        _ <-
          if (markAsReserved) markUTXOsAsReserved(filtered.map(_._1))
          else Future.unit
      } yield filtered

    val resultF = for {
      selectedUtxos <- selectedUtxosF
      change <- getNewChangeAddress(fromAccount)
      utxoSpendingInfos = {
        selectedUtxos.map { case (utxo, prevTx) =>
          utxo.toUTXOInfo(keyManager = self.keyManager, prevTx)
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
        ShufflingNonInteractiveFinalizer.txBuilderFrom(destinations,
                                                       utxoSpendingInfos,
                                                       feeRate,
                                                       change.scriptPubKey)

      (txBuilder, utxoSpendingInfos)
    }

    resultF.recoverWith { case NonFatal(error) =>
      logger.error(
        s"Failed to reserve utxos for amount=${amt} feeRate=$feeRate, unreserving the selected utxos")
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
