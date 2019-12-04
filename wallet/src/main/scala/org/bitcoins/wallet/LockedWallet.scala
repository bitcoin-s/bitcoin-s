package org.bitcoins.wallet

import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateAll}
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.node.NodeApi
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.keymanager.ReadMnemonicError.{
  DecryptionError,
  JsonParsingError
}
import org.bitcoins.keymanager.{
  ReadMnemonicError,
  ReadMnemonicSuccess,
  WalletStorage
}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.internal._
import org.bitcoins.wallet.models._

import scala.concurrent.{ExecutionContext, Future}

abstract class LockedWallet
    extends LockedWalletApi
    with UtxoHandling
    with AddressHandling
    with AccountHandling
    with TransactionProcessing {

  private[wallet] val addressDAO: AddressDAO = AddressDAO()
  private[wallet] val accountDAO: AccountDAO = AccountDAO()
  private[wallet] val spendingInfoDAO: SpendingInfoDAO = SpendingInfoDAO()

  /** Sums up the value of all unspent
    * TXOs in the wallet, filtered by the given predicate */
  private def filterThenSum(
      predicate: SpendingInfoDb => Boolean): Future[CurrencyUnit] = {
    for (utxos <- spendingInfoDAO.findAll())
      yield {
        val filtered = utxos
          .collect {
            case (txo) if !txo.spent && predicate(txo) =>
              txo.output.value
          }

        filtered.fold(0.sats)(_ + _)
      }
  }

  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    val confirmed = filterThenSum(_.confirmations > 0)
    confirmed.foreach(balance =>
      logger.trace(s"Confirmed balance=${balance.satoshis}"))
    confirmed
  }

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    val unconfirmed = filterThenSum(_.confirmations == 0)
    unconfirmed.foreach(balance =>
      logger.trace(s"Unconfirmed balance=${balance.satoshis}"))
    unconfirmed

  }

  /**
    * @inheritdoc
    */
  override def unlock(passphrase: AesPassword): UnlockWalletResult = {
    logger.debug(s"Trying to unlock wallet")
    val result =
      WalletStorage.decryptMnemonicFromDisk(walletConfig.seedPath, passphrase)
    result match {
      case DecryptionError =>
        logger.error(s"Bad password for unlocking wallet!")
        UnlockWalletError.BadPassword
      case JsonParsingError(message) =>
        logger.error(s"JSON parsing error when unlocking wallet: $message")
        UnlockWalletError.JsonParsingError(message)
      case ReadMnemonicError.NotFoundError =>
        logger.error(s"Encrypted mnemonic not found when unlocking the wallet!")
        UnlockWalletError.MnemonicNotFound

      case ReadMnemonicSuccess(mnemonic) =>
        logger.debug(s"Successfully uunlocked wallet")
        UnlockWalletSuccess(Wallet(mnemonic, nodeApi))
    }
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
  private case class LockedWalletImpl(override val nodeApi: NodeApi)(
      implicit val ec: ExecutionContext,
      val walletConfig: WalletAppConfig)
      extends LockedWallet {}

  def apply(nodeApi: NodeApi)(
      implicit ec: ExecutionContext,
      config: WalletAppConfig): LockedWallet = LockedWalletImpl(nodeApi)

}
