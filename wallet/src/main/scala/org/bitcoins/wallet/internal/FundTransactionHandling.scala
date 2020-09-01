package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.api.wallet.{
  AddressInfo,
  CoinSelectionAlgo,
  CoinSelector
}
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
import org.bitcoins.crypto.Sign
import org.bitcoins.keymanager.bip39.BIP39KeyManager
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
                               keyManagerOpt = None,
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
      keyManagerOpt: Option[BIP39KeyManager],
      coinSelectionAlgo: CoinSelectionAlgo =
        CoinSelectionAlgo.AccumulateLargest,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean = false): Future[(
      RawTxBuilderWithFinalizer[ShufflingNonInteractiveFinalizer],
      Vector[ScriptSignatureParams[InputInfo]])] = {
    def utxosF: Future[Vector[SpendingInfoDb]] =
      for {
        utxos <- fromTagOpt match {
          case None =>
            listUtxos(fromAccount.hdAccount)
          case Some(tag) =>
            listUtxos(fromAccount.hdAccount, tag)
        }

        // Need to remove immature coinbase inputs
        coinbaseUtxos = utxos.filter(_.outPoint == EmptyTransactionOutPoint)
        confFs = coinbaseUtxos.map(utxo =>
          chainQueryApi
            .getNumberOfConfirmations(utxo.blockHash.get)
            .map((utxo, _)))
        confs <- FutureUtil.collect(confFs)
        immatureCoinbases =
          confs
            .filter {
              case (_, confsOpt) =>
                confsOpt.isDefined && confsOpt.get > Consensus.coinbaseMaturity
            }
            .map(_._1)
      } yield utxos.diff(immatureCoinbases)

    val selectedUtxosF: Future[Vector[SpendingInfoDb]] =
      for {
        walletUtxos <- utxosF
        utxos = CoinSelector.selectByAlgo(coinSelectionAlgo = coinSelectionAlgo,
                                          walletUtxos = walletUtxos,
                                          outputs = destinations,
                                          feeRate = feeRate)
      } yield utxos

    val addrInfosWithUtxoF: Future[
      Vector[(SpendingInfoDb, Transaction, AddressInfo)]] =
      for {
        selectedUtxos <- selectedUtxosF
        _ = selectedUtxosF.failed.foreach(err =>
          logger.error("Error selecting utxos to fund transaction ", err))
        addrInfoOptF = selectedUtxos.map { utxo =>
          // .gets should be safe here because of foreign key at the database level
          for {
            addrInfo <- getAddressInfo(utxo, networkParameters).map(_.get)
            prevTx <-
              transactionDAO
                .findByOutPoint(utxo.outPoint)
                .map(_.get.transaction)
          } yield (utxo, prevTx, addrInfo)
        }
        vec <- FutureUtil.collect(addrInfoOptF).map(_.toVector)
      } yield vec

    val resultF = for {
      addrInfosWithUtxo <- addrInfosWithUtxoF
      change <- getNewChangeAddress(fromAccount)
      utxoSpendingInfos = {
        addrInfosWithUtxo.map {
          case (utxo, prevTx, addrInfo) =>
            keyManagerOpt match {
              case Some(km) =>
                utxo.toUTXOInfo(keyManager = km, prevTx)
              case None =>
                utxo.toUTXOInfo(sign = Sign.dummySign(addrInfo.pubkey), prevTx)
            }

        }
      }
      _ <-
        if (markAsReserved) markUTXOsAsReserved(addrInfosWithUtxo.map(_._1))
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
            _ <- unmarkUTXOsAsReserved(utxos)
          } yield error
        } else Future.failed(error)
    }

    resultF
  }
}
