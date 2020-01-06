package org.bitcoins.wallet

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateAll}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint, TransactionOutput}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.internal._
import org.bitcoins.wallet.models.{SpendingInfoDb, _}

import scala.concurrent.{ExecutionContext, Future}

abstract class LockedWallet
    extends LockedWalletApi
    with UtxoHandling
    with AddressHandling
    with AccountHandling
    with FundTransactionHandling
    with TransactionProcessing
    with RescanHandling {

  private[wallet] val addressDAO: AddressDAO = AddressDAO()
  private[wallet] val accountDAO: AccountDAO = AccountDAO()
  private[wallet] val spendingInfoDAO: SpendingInfoDAO = SpendingInfoDAO()

  override def isEmpty(): Future[Boolean] =
    for {
      addressCount <- addressDAO.count()
      spendingInfoCount <- spendingInfoDAO.count()
    } yield addressCount == 0 && spendingInfoCount == 0

  /** Sums up the value of all unspent
    * TXOs in the wallet, filtered by the given predicate */
  private def filterThenSum(
      predicate: SpendingInfoDb => Boolean): Future[CurrencyUnit] = {
    for (utxos <- spendingInfoDAO.findAll())
      yield {
        val filtered = utxos
          .filter(predicate)
          .map {
            case txo: SpendingInfoDb =>
              txo.state match {
                case TxoState.PendingConfirmationsReceived |
                    TxoState.ConfirmedReceived =>
                  txo.output.value
                case TxoState.PendingConfirmationsSpent |
                    TxoState.ConfirmedSpent | TxoState.DoesNotExist =>
                  CurrencyUnits.zero
              }
          }

        filtered.fold(0.sats)(_ + _)
      }
  }

  override def fundRawTransaction(
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
    * If you pass in a [[KeyManager]], the [[org.bitcoins.core.wallet.utxo.UTXOSpendingInfo.signers signers]]
    * will be populated with really signers that can be used to produce valid [[org.bitcoins.core.crypto.ECDigitalSignature signatures]]
    *
    * If you do not pass in a key manager, the transaction will contain [[org.bitcoins.core.protocol.script.EmptyScriptSignature EmptyScriptSignature]]
    * */
  private[wallet] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      keyManagerOpt: Option[BIP39KeyManager]): Future[BitcoinTxBuilder] = {
    val utxosF = listUtxos()
    val changeAddrF = getNewChangeAddress(fromAccount)
    val selectedUtxosF = for {
      walletUtxos <- utxosF
      change <- changeAddrF
      //currently just grab the biggest utxos
      selectedUtxos = CoinSelector
        .accumulateLargest(walletUtxos, destinations, feeRate)
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

  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    val confirmed = filterThenSum(_.blockHash.isDefined)
    confirmed.foreach(balance =>
      logger.trace(s"Confirmed balance=${balance.satoshis}"))
    confirmed
  }

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    val unconfirmed = filterThenSum(_.blockHash.isEmpty)
    unconfirmed.foreach(balance =>
      logger.trace(s"Unconfirmed balance=${balance.satoshis}"))
    unconfirmed

  }

  /** Enumerates all the TX outpoints in the wallet  */
  protected[wallet] def listOutpoints(): Future[Vector[TransactionOutPoint]] =
    spendingInfoDAO.findAllOutpoints()

  /** Gets the size of the bloom filter for this wallet  */
  private def getBloomFilterSize(
      pubkeys: Seq[ECPublicKey],
      outpoints: Seq[TransactionOutPoint]): Int = {
    // when a public key is inserted into a filter
    // both the pubkey and the hash of the pubkey
    // gets inserted
    pubkeys.length * 2
  } + outpoints.length

  // todo: insert TXIDs? need to track which txids we should
  // ask for, somehow
  // We add all outpoints to the bloom filter as a way
  // of working around the fact that bloom filters
  // was never updated to incorporate SegWit changes.
  // see this mailing list thread for context:
  //   https://www.mail-archive.com/bitcoin-dev@lists.linuxfoundation.org/msg06950.html
  // especially this email from Jim Posen:
  //   https://www.mail-archive.com/bitcoin-dev@lists.linuxfoundation.org/msg06952.html
  override def getBloomFilter(): Future[BloomFilter] = {
    for {
      pubkeys <- listPubkeys()
      outpoints <- listOutpoints()
    } yield {
      val filterSize = getBloomFilterSize(pubkeys, outpoints)

      // todo: Is this the best flag to use?
      val bloomFlag = BloomUpdateAll

      val baseBloom =
        BloomFilter(numElements = filterSize,
                    falsePositiveRate = walletConfig.bloomFalsePositiveRate,
                    flags = bloomFlag)

      val withPubs = pubkeys.foldLeft(baseBloom) { _.insert(_) }
      outpoints.foldLeft(withPubs) { _.insert(_) }
    }
  }
}

object LockedWallet {
  private case class LockedWalletImpl(
      override val nodeApi: NodeApi,
      override val chainQueryApi: ChainQueryApi)(
      implicit val ec: ExecutionContext,
      val walletConfig: WalletAppConfig)
      extends LockedWallet {}

  def apply(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit ec: ExecutionContext,
      config: WalletAppConfig): LockedWallet =
    LockedWalletImpl(nodeApi, chainQueryApi)

}
