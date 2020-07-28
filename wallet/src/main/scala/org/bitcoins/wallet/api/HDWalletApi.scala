package org.bitcoins.wallet.api

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType, HDPurpose}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.keymanager.KeyManagerParams
import org.bitcoins.wallet.models.{AccountDb, AddressDb, SpendingInfoDb}

import scala.concurrent.{ExecutionContext, Future}

/**
  * API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait HDWalletApi extends WalletApi {

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

  /** Generates a new change address */
  protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress]

  override def getNewChangeAddress()(implicit
      ec: ExecutionContext): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccount()
      addr <- getNewChangeAddress(account)
    } yield addr
  }

  /**
    * Fetches the default account from the DB
    * @return Future[AccountDb]
    */
  protected[wallet] def getDefaultAccount(): Future[AccountDb]

  /** Fetches the default account for the given address/account kind
    * @param addressType
    */
  protected[wallet] def getDefaultAccountForType(
      addressType: AddressType): Future[AccountDb]

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

  /**
    * Sends money from the specified account
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

  /**
    * Sends money from the specified account
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

  /**
    * Sends money from the specified account
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

  /**
    * Sends money from the specified account
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

  def listDefaultAccountUtxos(): Future[Vector[SpendingInfoDb]] =
    listUtxos(walletConfig.defaultAccount)

  def listUtxos(account: HDAccount): Future[Vector[SpendingInfoDb]]

  def listUtxos(
      hdAccount: HDAccount,
      tag: AddressTag): Future[Vector[SpendingInfoDb]]

  def listUtxos(
      hdAccount: HDAccount,
      state: TxoState): Future[Vector[SpendingInfoDb]]

  def listAddresses(account: HDAccount): Future[Vector[AddressDb]]

  def listSpentAddresses(account: HDAccount): Future[Vector[AddressDb]]

  def listFundedAddresses(
      account: HDAccount): Future[Vector[(AddressDb, CurrencyUnit)]]

  def listUnusedAddresses(account: HDAccount): Future[Vector[AddressDb]]

  override def clearAllUtxosAndAddresses(): Future[HDWalletApi]

  def clearUtxosAndAddresses(account: HDAccount): Future[HDWalletApi]

  /** Gets the address associated with the pubkey at
    * the resulting `BIP32Path` determined by the
    * default account and the given chainType and addressIndex
    */
  def getAddress(chainType: HDChainType, addressIndex: Int)(implicit
      ec: ExecutionContext): Future[AddressDb] = {
    for {
      account <- getDefaultAccount()
      address <- getAddress(account, chainType, addressIndex)
    } yield address
  }

  /** Gets the address associated with the pubkey at
    * the resulting `BIP32Path` determined the given
    * account, chainType, and addressIndex
    */
  def getAddress(
      account: AccountDb,
      chainType: HDChainType,
      addressIndex: Int): Future[AddressDb]

  def listAccounts(): Future[Vector[AccountDb]]

  /** Lists all wallet accounts with the given type
    * @param purpose
    * @return [[Future[Vector[AccountDb]]
    */
  def listAccounts(purpose: HDPurpose)(implicit
      ec: ExecutionContext): Future[Vector[AccountDb]] =
    listAccounts().map(_.filter(_.hdAccount.purpose == purpose))

  def rescanNeutrinoWallet(
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean): Future[Unit]

  override def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean)(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      account <- getDefaultAccount()
      _ <- rescanNeutrinoWallet(account.hdAccount,
                                startOpt,
                                endOpt,
                                addressBatchSize,
                                useCreationTime)
    } yield ()
  }

  def fullRescanNeutrinoWallet(
      account: HDAccount,
      addressBatchSize: Int): Future[Unit] = {
    rescanNeutrinoWallet(account = account,
                         startOpt = None,
                         endOpt = None,
                         addressBatchSize = addressBatchSize,
                         useCreationTime = false)
  }

  /** Helper method to rescan the ENTIRE blockchain. */
  override def fullRescanNeutrinoWallet(addressBatchSize: Int)(implicit
      ec: ExecutionContext): Future[Unit] = {
    for {
      account <- getDefaultAccount()
      _ <- fullRescanNeutrinoWallet(account.hdAccount, addressBatchSize)
    } yield ()
  }

  def createNewAccount(keyManagerParams: KeyManagerParams): Future[HDWalletApi]

  /**
    * Tries to create a new account in this wallet. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  def createNewAccount(
      hdAccount: HDAccount,
      keyManagerParams: KeyManagerParams): Future[HDWalletApi]

}
