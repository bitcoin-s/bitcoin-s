package org.bitcoins.wallet.internal

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.{
  EmptyTransactionOutPoint,
  InputUtil,
  Transaction,
  TransactionOutput,
  TxUtil
}
import org.bitcoins.core.wallet.builder.{
  NonInteractiveWithChangeFinalizer,
  RawTxBuilder,
  RawTxBuilderWithFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto.Sign
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.WalletLogger
import org.bitcoins.wallet.api.{AddressInfo, CoinSelector, WalletApi}
import org.bitcoins.wallet.models.{AccountDb, SpendingInfoDb}

import scala.concurrent.Future

trait FundTransactionHandling extends WalletLogger { self: WalletApi =>

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      markAsReserved: Boolean): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      funded <- fundRawTransaction(destinations = destinations,
                                   feeRate = feeRate,
                                   fromAccount = account,
                                   markAsReserved = markAsReserved)
    } yield funded
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      markAsReserved: Boolean = false): Future[Transaction] = {
    val txBuilderF =
      fundRawTransactionInternal(destinations = destinations,
                                 feeRate = feeRate,
                                 fromAccount = fromAccount,
                                 keyManagerOpt = None,
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
    * */
  private[wallet] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      keyManagerOpt: Option[BIP39KeyManager],
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.AccumulateLargest,
      markAsReserved: Boolean = false): Future[
    (RawTxBuilderWithFinalizer, Vector[ScriptSignatureParams[InputInfo]])] = {
    val utxosF = for {
      utxos <- listUtxos(fromAccount.hdAccount)

      // Need to remove immature coinbase inputs
      coinbaseUtxos = utxos.filter(_.outPoint == EmptyTransactionOutPoint)
      confFs = coinbaseUtxos.map(
        utxo =>
          chainQueryApi
            .getNumberOfConfirmations(utxo.blockHash.get)
            .map((utxo, _)))
      confs <- Future.sequence(confFs)
      immatureCoinbases = confs
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
      selectedUtxos <- if (markAsReserved) markUTXOsAsReserved(utxos)
      else Future.successful(utxos)
    } yield selectedUtxos

    val addrInfosWithUtxoF: Future[Vector[(SpendingInfoDb, AddressInfo)]] =
      for {
        selectedUtxos <- selectedUtxosF
        addrInfoOptF = selectedUtxos.map { utxo =>
          val addrInfoOptF = getAddressInfo(utxo)
          //.get should be safe here because of foreign key at the database level
          addrInfoOptF.map(addrInfoOpt => (utxo, addrInfoOpt.get))
        }
        vec <- Future.sequence(addrInfoOptF)
      } yield vec

    val txBuilderF = for {
      addrInfosWithUtxo <- addrInfosWithUtxoF
      change <- getNewChangeAddress(fromAccount)
      utxoSpendingInfos = {
        addrInfosWithUtxo.map {
          case (utxo, addrInfo) =>
            keyManagerOpt match {
              case Some(km) =>
                utxo.toUTXOInfo(keyManager = km)
              case None =>
                utxo.toUTXOInfo(sign = Sign.dummySign(addrInfo.pubkey))
            }

        }
      }
    } yield {
      logger.info({
        val utxosStr = utxoSpendingInfos
          .map { utxo =>
            import utxo.outPoint
            s"${outPoint.txId.hex}:${outPoint.vout.toInt}"
          }
          .mkString(", ")
        s"Spending UTXOs: $utxosStr"
      })

      utxoSpendingInfos.zipWithIndex.foreach {
        case (utxo, index) =>
          logger.info(s"UTXO $index details: ${utxo.output}")
      }

      val inputs =
        InputUtil.calcSequenceForInputs(utxoSpendingInfos, Policy.isRBFEnabled)

      val lockTime = TxUtil.calcLockTime(utxoSpendingInfos).get

      val txBuilder = RawTxBuilder().setLockTime(lockTime) ++= destinations ++= inputs

      val finalizer = NonInteractiveWithChangeFinalizer(
        utxoSpendingInfos.map(_.inputInfo),
        feeRate,
        change.scriptPubKey)

      (txBuilder.setFinalizer(finalizer), utxoSpendingInfos)
    }

    txBuilderF
  }
}
