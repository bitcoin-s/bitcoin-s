package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.wallet.db.{AccountDb, SpendingInfoDb}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{AddressType, HDAccount, HDPurpose}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.builder.{
  FundRawTxHelper,
  ShufflingNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.AddressTag

import scala.concurrent.{ExecutionContext, Future}

/** API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait HDWalletApi extends WalletApi {

  override def keyManager: BIP39KeyManagerApi
  def accountHandling: AccountHandlingApi
  def fundTxHandling: FundTransactionHandlingApi
  def rescanHandling: RescanHandlingApi
  def addressHandling: AddressHandlingApi

  /** Gets the balance of the given account */
  def getBalance(account: HDAccount)(implicit
      ec: ExecutionContext): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance(account)
    val unconfirmedF = getUnconfirmedBalance(account)
    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield {
      confirmed + unconfirmed
    }
  }

  def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit]

  def getUnconfirmedBalance(account: HDAccount): Future[CurrencyUnit]

  /** Fetches the default account from the DB
    * @return
    *   Future[AccountDb]
    */
  def getDefaultAccount(): Future[AccountDb] =
    accountHandling.getDefaultAccount()

  /** Fetches the default account for the given address/account kind
    * @param addressType
    */
  def getDefaultAccountForType(addressType: AddressType): Future[AccountDb]

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction]

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendWithAlgo(address, amount, feeRate, algo, fromAccount, Vector.empty)

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendWithAlgo(address, amount, feeRate, algo, fromAccount)
    } yield tx
  }

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendWithAlgo(address, amount, feeRateOpt, algo, account)
    } yield tx
  }

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendWithAlgo(address, amount, feeRate, algo, account)
    } yield tx
  }

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendWithAlgo(address, amount, feeRate, algo, account, newTags)
    } yield tx
  }

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction]

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendFromOutPoints(outPoints,
                      address,
                      amount,
                      feeRate,
                      fromAccount,
                      Vector.empty)

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendFromOutPoints(outPoints, address, amount, feeRate, fromAccount)
    } yield tx
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendFromOutPoints(outPoints, address, amount, feeRateOpt, account)
    } yield tx
  }

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendFromOutPoints(outPoints, address, amount, feeRate, account)
    } yield tx
  }

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <-
        sendFromOutPoints(outPoints, address, amount, feeRate, account, newTags)
    } yield tx
  }

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction]

  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendToAddress(address, amount, feeRate, fromAccount, Vector.empty)

  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToAddress(address, amount, feeRate, fromAccount)
    } yield tx
  }

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddress(address, amount, feeRateOpt, account)
    } yield tx
  }

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddress(address, amount, feeRate, account)
    } yield tx
  }

  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddress(address, amount, feeRate, account, newTags)
    } yield tx
  }

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction]

  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendToAddresses(addresses, amounts, feeRate, fromAccount, Vector.empty)

  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToAddresses(addresses, amounts, feeRate, fromAccount)
    } yield tx
  }

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddresses(addresses, amounts, feeRateOpt, account)
    } yield tx
  }

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddresses(addresses, amounts, feeRate, account)
    } yield tx
  }

  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddresses(addresses, amounts, feeRate, account, newTags)
    } yield tx
  }

  /** Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction]

  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] =
    sendToOutputs(outputs, feeRate, fromAccount, Vector.empty)

  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToOutputs(outputs, feeRate, fromAccount)
    } yield tx
  }

  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToOutputs(outputs, feeRate, account, newTags)
    } yield tx
  }

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToOutputs(outputs, feeRateOpt, account)
    } yield tx
  }

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToOutputs(outputs, feeRate, account)
    } yield tx
  }

  def signPSBT(psbt: PSBT)(implicit ec: ExecutionContext): Future[PSBT]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate, fromAccount)
    } yield tx
  }

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate, account)
    } yield tx
  }

  def listDefaultAccountUtxos(): Future[Vector[SpendingInfoDb]]

  def listUtxos(hdAccount: HDAccount): Future[Vector[SpendingInfoDb]]

  override def clearAllUtxos(): Future[HDWalletApi]

  def listAccounts(): Future[Vector[AccountDb]] = {
    accountHandling.listAccounts()
  }

  /** Lists all wallet accounts with the given type
    * @param purpose
    * @return
    *   [[Future[Vector[AccountDb]]
    */
  def listAccounts(purpose: HDPurpose)(implicit
      ec: ExecutionContext): Future[Vector[AccountDb]] = {
    accountHandling.listAccounts().map(_.filter(_.hdAccount.purpose == purpose))
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      markAsReserved: Boolean)
      : Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundTxHandling.fundRawTransaction(destinations,
                                      feeRate,
                                      fromAccount,
                                      markAsReserved)
  }
}
