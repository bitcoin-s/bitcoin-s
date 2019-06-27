package org.bitcoins.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.ReadMnemonicError.DecryptionError
import org.bitcoins.wallet.ReadMnemonicError.JsonParsingError
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.bloom.BloomUpdateAll
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.wallet.internal._

abstract class LockedWallet
    extends LockedWalletApi
    with UtxoHandling
    with AddressHandling
    with AccountHandling
    with TransactionProcessing
    with BitcoinSLogger {

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
    val result = WalletStorage.decryptMnemonicFromDisk(passphrase)
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
        UnlockWalletSuccess(Wallet(mnemonic))
    }
  }

  /** Gets the size of the bloom filter for this wallet  */
  private def getBloomFilterSize(): Future[Int] = {
    for {
      pubkeys <- listPubkeys()
    } yield {
      // when a public key is inserted into a filter
      // both the pubkey and the hash of the pubkey
      // gets inserted
      pubkeys.length * 2
    }

  }

  // todo: insert TXIDs? need to track which txids we should
  // ask for, somehow
  override def getBloomFilter(): Future[BloomFilter] = {
    for {
      pubkeys <- listPubkeys()
      filterSize <- getBloomFilterSize()
    } yield {

      // todo: Is this the best flag to use?
      val bloomFlag = BloomUpdateAll

      val baseBloom =
        BloomFilter(numElements = filterSize,
                    falsePositiveRate = walletConfig.bloomFalsePositiveRate,
                    flags = bloomFlag)

      pubkeys.foldLeft(baseBloom) { _.insert(_) }
    }
  }
}

object LockedWallet {
  private case class LockedWalletImpl()(
      implicit val ec: ExecutionContext,
      val walletConfig: WalletAppConfig)
      extends LockedWallet

  def apply()(
      implicit ec: ExecutionContext,
      config: WalletAppConfig): LockedWallet = LockedWalletImpl()

}
