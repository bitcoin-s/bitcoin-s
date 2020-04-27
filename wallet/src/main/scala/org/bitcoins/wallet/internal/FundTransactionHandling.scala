package org.bitcoins.wallet.internal

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{
  RawTxBuilder,
  RawTxBuilderWithFinalizer,
  StandardNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.Sign
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.api.{AddressInfo, CoinSelector}
import org.bitcoins.wallet.models.{AccountDb, SpendingInfoDb}
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.Future

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
    val txBuilderF =
      fundRawTransactionInternal(destinations = destinations,
                                 feeRate = feeRate,
                                 fromAccount = fromAccount,
                                 keyManagerOpt = None,
                                 fromTagOpt = fromTagOpt,
                                 markAsReserved = markAsReserved)
    txBuilderF.flatMap(_._1.buildTx())
  }

  /** This returns a [[RawTxBuilder]] that can be used to generate an unsigned transaction with [[RawTxBuilder.result()]]
    * which can be used with signing.
    *
    * If you pass in a [[org.bitcoins.keymanager.KeyManager]], the [[org.bitcoins.core.wallet.utxo.ScriptSignatureParams.signers signers]]
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
      RawTxBuilderWithFinalizer[StandardNonInteractiveFinalizer],
      Vector[ScriptSignatureParams[InputInfo]])] = {
    val utxosF = for {
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
      confs <- Future.sequence(confFs)
      immatureCoinbases =
        confs
          .filter {
            case (_, confsOpt) =>
              confsOpt.isDefined && confsOpt.get > Consensus.coinbaseMaturity
          }
          .map(_._1)
    } yield utxos.diff(immatureCoinbases)

    val selectedUtxosF = for {
      walletUtxos <- utxosF
      //currently just grab the biggest utxos
      utxos = CoinSelector.selectByAlgo(coinSelectionAlgo = coinSelectionAlgo,
                                        walletUtxos = walletUtxos,
                                        outputs = destinations,
                                        feeRate = feeRate)
      selectedUtxos <-
        if (markAsReserved) markUTXOsAsReserved(utxos)
        else Future.successful(utxos)
    } yield selectedUtxos

    val addrInfosWithUtxoF: Future[
      Vector[(SpendingInfoDb, Transaction, AddressInfo)]] =
      for {
        selectedUtxos <- selectedUtxosF
        _ = selectedUtxosF.failed.foreach(err =>
          logger.error("Error selecting utxos to fund transaction ", err))
        addrInfoOptF = selectedUtxos.map { utxo =>
          // .gets should be safe here because of foreign key at the database level
          for {
            addrInfo <- getAddressInfo(utxo).map(_.get)
            prevTx <-
              transactionDAO
                .findByOutPoint(utxo.outPoint)
                .map(_.get.transaction)
          } yield (utxo, prevTx, addrInfo)
        }
        vec <- Future.sequence(addrInfoOptF)
      } yield vec

    val txBuilderF = for {
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

      val finalizer = StandardNonInteractiveFinalizer(
        utxoSpendingInfos.map(_.inputInfo),
        feeRate,
        change.scriptPubKey)

      (txBuilder.setFinalizer(finalizer), utxoSpendingInfos)
    }

    txBuilderF
  }
}
