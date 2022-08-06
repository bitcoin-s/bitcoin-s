package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder._
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.Future
import scala.util.control.NonFatal

trait FundTransactionHandling extends WalletLogger { self: Wallet =>

  def changeCost(feeUnit: FeeUnit): CurrencyUnit = {
    walletConfig.defaultAddressType match {
      case AddressType.SegWit       => feeUnit * 31
      case AddressType.NestedSegWit => feeUnit * 43
      case AddressType.Legacy       => feeUnit * 34
    }
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean): Future[
    FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
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
      markAsReserved: Boolean = false): Future[
    FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundRawTransactionInternal(destinations = destinations,
                               feeRate = feeRate,
                               fromAccount = fromAccount,
                               fromTagOpt = fromTagOpt,
                               markAsReserved = markAsReserved)
  }

  /** Funds an unsigned transaction from the specified account */
  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      markAsReserved: Boolean): Future[
    FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundRawTransaction(destinations = destinations,
                       feeRate = feeRate,
                       fromAccount = fromAccount,
                       fromTagOpt = None,
                       markAsReserved = markAsReserved)
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
      markAsReserved: Boolean): Future[
    FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    val amts = destinations.map(_.value)
    //need to allow 0 for OP_RETURN outputs
    require(amts.forall(_.satoshis.toBigInt >= 0),
            s"Cannot fund a transaction for a negative amount, got=$amts")
    val amt = amts.sum
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
          .map(CoinSelectorUtxo.fromSpendingInfoDb)

        utxos = CoinSelector.selectByAlgo(
          coinSelectionAlgo = coinSelectionAlgo,
          walletUtxos = selectableUtxos,
          outputs = destinations,
          feeRate = feeRate,
          changeCostOpt = Some(changeCost(feeRate)),
          longTermFeeRateOpt = Some(self.walletConfig.longTermFeeRate)
        )
        filtered = walletUtxos.filter(utxo =>
          utxos.exists(_.outPoint == utxo._1.outPoint))
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

      FundRawTxHelper(txBuilderWithFinalizer = txBuilder,
                      scriptSigParams = utxoSpendingInfos,
                      feeRate)
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
