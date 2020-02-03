package org.bitcoins.wallet.internal

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.WalletLogger
import org.bitcoins.wallet.api.{AddressInfo, CoinSelector, LockedWalletApi}
import org.bitcoins.wallet.models.{AccountDb, SpendingInfoDb}

import scala.concurrent.Future

trait FundTransactionHandling extends WalletLogger { self: LockedWalletApi =>

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      funded <- fundRawTransaction(destinations = destinations,
                                   feeRate = feeRate,
                                   fromAccount = account)
    } yield funded
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb): Future[Transaction] = {
    val txBuilderF =
      fundRawTransactionInternal(destinations = destinations,
                                 feeRate = feeRate,
                                 fromAccount = fromAccount,
                                 keyManagerOpt = None)
    txBuilderF.flatMap(_.unsignedTx)
  }

  /** This returns a [[BitcoinTxBuilder]] that can be used to generate a unsigned transaction with [[BitcoinTxBuilder.unsignedTx unsignedTx]]
    * which can be used with signing, or you can just directly call [[BitcoinTxBuilder.sign sign]] to sign the transaction with this instance
    * of [[BitcoinTxBuilder]]
    *
    * If you pass in a [[org.bitcoins.keymanager.KeyManager]], the [[org.bitcoins.core.wallet.utxo.UTXOSpendingInfo.signers signers]]
    * will be populated with valid signers that can be used to produce valid [[org.bitcoins.core.crypto.ECDigitalSignature signatures]]
    *
    * If you do not pass in a key manager, the transaction built by [[BitcoinTxBuilder txbuilder]] will contain [[org.bitcoins.core.protocol.script.EmptyScriptSignature EmptyScriptSignature]]
    *
    * Currently utxos are funded with [[CoinSelector.accumulateLargest() accumulateLargest]] coin seleciton algorithm
    * */
  private[wallet] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      keyManagerOpt: Option[BIP39KeyManager]): Future[BitcoinTxBuilder] = {
    val utxosF = listUtxos(fromAccount.hdAccount)
    val changeAddrF = getNewChangeAddress(fromAccount)
    val selectedUtxosF = for {
      walletUtxos <- utxosF
      //currently just grab the biggest utxos
      selectedUtxos = CoinSelector
        .accumulateLargest(walletUtxos, destinations, feeRate)
    } yield selectedUtxos

    val addrInfosWithUtxoF: Future[Vector[(SpendingInfoDb, AddressInfo)]] =
      for {
        selectedUtxos <- selectedUtxosF
        _ <- Future {
          selectedUtxosF.failed.foreach(err =>
            logger.error(err.getLocalizedMessage))
        }
        addrInfoOptF = selectedUtxos.map { utxo =>
          val addrInfoOptF = getAddressInfo(utxo)
          //.get should be safe here because of foreign key at the database level
          addrInfoOptF.map(addrInfoOpt => (utxo, addrInfoOpt.get))
        }
        vec <- Future.sequence(addrInfoOptF)
      } yield vec

    val txBuilderF = for {
      addrInfosWithUtxo <- addrInfosWithUtxoF
      change <- changeAddrF
      utxoSpendingInfos = {
        addrInfosWithUtxo.map {
          case (utxo, addrInfo) =>
            keyManagerOpt match {
              case Some(km) =>
                utxo.toUTXOSpendingInfo(account = fromAccount,
                                        keyManager = km,
                                        networkParameters = networkParameters)
              case None =>
                utxo.toUTXOSpendingInfo(account = fromAccount,
                                        sign = Sign.dummySign(addrInfo.pubkey),
                                        networkParameters = networkParameters)
            }

        }
      }
      txBuilder <- {

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

        networkParameters match {
          case b: BitcoinNetwork =>
            BitcoinTxBuilder(destinations = destinations,
                             utxos = utxoSpendingInfos,
                             feeRate = feeRate,
                             changeSPK = change.scriptPubKey,
                             network = b)
        }

      }
    } yield {
      txBuilder
    }

    txBuilderF
  }
}
