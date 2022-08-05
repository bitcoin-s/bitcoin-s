package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet._
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder._
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.Future

trait FundTransactionHandling extends WalletLogger { self: Wallet =>
  import walletConfig.profile.api._

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
    val action = fundRawTransactionInternalAction(destinations,
                                                  feeRate,
                                                  fromAccount,
                                                  coinSelectionAlgo,
                                                  fromTagOpt,
                                                  markAsReserved)
    safeDatabase.run(action)
  }

  private[bitcoins] def fundRawTransactionInternalAction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.LeastWaste,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean): DBIOAction[
    FundRawTxHelper[ShufflingNonInteractiveFinalizer],
    NoStream,
    Effect.Read with Effect.Write with Effect.Transactional] = {
    val amts = destinations.map(_.value)
    //need to allow 0 for OP_RETURN outputs
    require(amts.forall(_.satoshis.toBigInt >= 0),
            s"Cannot fund a transaction for a negative amount, got=$amts")
    val amt = amts.sum
    logger.info(s"Attempting to fund a tx for amt=$amt with feeRate=$feeRate")
    val utxosA =
      for {
        utxos <- fromTagOpt match {
          case None =>
            spendingInfoDAO.findAllUnspentForAccountAction(
              fromAccount.hdAccount)
          case Some(tag) =>
            spendingInfoDAO.findAllUnspentForTagAction(tag).map { utxos =>
              utxos.filter(utxo =>
                HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                                        account = fromAccount.hdAccount))
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
          _._1.state == TxoState.ImmatureCoinbase)
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
          longTermFeeRateOpt = Some(self.walletConfig.longTermFeeRate)
        )
        filtered = walletUtxos.filter(utxo =>
          utxos.exists(_.outPoint == utxo._1.outPoint))
        _ <-
          if (markAsReserved) markUTXOsAsReservedAction(filtered.map(_._1))
          else DBIO.successful(())
      } yield filtered

    for {
      selectedUtxos <- selectedUtxosA
      change <- getNewChangeAddressAction(fromAccount)
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
  }
}
