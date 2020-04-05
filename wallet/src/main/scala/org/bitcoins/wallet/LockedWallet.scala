package org.bitcoins.wallet

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateAll}
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.utxo.TxoState
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
  private[wallet] val transactionDAO: TransactionDAO = TransactionDAO()
  private[wallet] val incomingTxDAO: IncomingTransactionDAO =
    IncomingTransactionDAO()
  private[wallet] val outgoingTxDAO: OutgoingTransactionDAO =
    OutgoingTransactionDAO()

  private[wallet] val ptlcDAO = PTLCDAO()
  private[wallet] val ptlcInvoiceDAO = PTLCInvoiceDAO()
  private[wallet] val ptlcAcceptDAO = PTLCAcceptDAO()
  private[wallet] val ptlcFundingInputsDAO = PTLCFundingInputDAO()

  override def isEmpty(): Future[Boolean] =
    for {
      addressCount <- addressDAO.count()
      spendingInfoCount <- spendingInfoDAO.count()
    } yield addressCount == 0 && spendingInfoCount == 0

  override def clearUtxosAndAddresses(): Future[LockedWallet] = {
    for {
      _ <- spendingInfoDAO.deleteAll()
      _ <- addressDAO.deleteAll()
    } yield this
  }

  /** Sums up the value of all unspent
    * TXOs in the wallet, filtered by the given predicate */
  private def filterThenSum(
      predicate: SpendingInfoDb => Boolean): Future[CurrencyUnit] = {
    for (utxos <- spendingInfoDAO.findAllUnspentForAccount(
           walletConfig.defaultAccount))
      yield {
        val filtered = utxos
          .filter(predicate)
          .map {
            case txo: SpendingInfoDb =>
              txo.state match {
                case TxoState.PendingConfirmationsReceived |
                    TxoState.ConfirmedReceived =>
                  txo.output.value
                case TxoState.Reserved | TxoState.PendingConfirmationsSpent |
                    TxoState.ConfirmedSpent | TxoState.DoesNotExist =>
                  CurrencyUnits.zero
              }
          }

        filtered.fold(0.sats)(_ + _)
      }
  }

  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    val confirmed = filterThenSum(_.blockHash.isDefined)
    confirmed.foreach(balance =>
      logger.trace(s"Confirmed balance=${balance.satoshis}"))
    confirmed
  }

  override def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] = {
    val allUnspentF = spendingInfoDAO.findAllUnspent()
    val unspentInAccountF = for {
      allUnspent <- allUnspentF
    } yield {
      allUnspent.filter { utxo =>
        HDAccount.isSameAccount(utxo.privKeyPath.path, account) &&
        utxo.blockHash.isDefined
      }
    }

    unspentInAccountF.map(_.foldLeft(CurrencyUnits.zero)(_ + _.output.value))
  }

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    val unconfirmed = filterThenSum(_.blockHash.isEmpty)
    unconfirmed.foreach(balance =>
      logger.trace(s"Unconfirmed balance=${balance.satoshis}"))
    unconfirmed

  }

  override def getUnconfirmedBalance(
      account: HDAccount): Future[CurrencyUnit] = {
    val allUnspentF = spendingInfoDAO.findAllUnspent()
    val unspentInAccountF = for {
      allUnspent <- allUnspentF
    } yield {
      allUnspent.filter { utxo =>
        HDAccount.isSameAccount(utxo.privKeyPath.path, account) &&
        utxo.blockHash.isEmpty
      }
    }

    unspentInAccountF.map(_.foldLeft(CurrencyUnits.zero)(_ + _.output.value))
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

  override def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    val updated = utxos.map(_.copyWithState(TxoState.Reserved))
    spendingInfoDAO.updateAll(updated)
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
