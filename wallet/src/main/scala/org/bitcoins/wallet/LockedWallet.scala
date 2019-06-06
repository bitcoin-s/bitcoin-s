package org.bitcoins.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.hd._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.blockchain._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.{BitcoinSLogger, EitherUtil}
import org.bitcoins.wallet.api.AddUtxoError.{AddressNotFound, BadSPK}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models._

import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.ReadMnemonicError.DecryptionError
import org.bitcoins.wallet.ReadMnemonicError.JsonParsingError
import org.bitcoins.wallet.config.WalletAppConfig

abstract class LockedWallet extends LockedWalletApi with BitcoinSLogger {

  protected val addressDAO: AddressDAO = AddressDAO()
  protected val accountDAO: AccountDAO = AccountDAO()
  protected val utxoDAO: UTXOSpendingInfoDAO = UTXOSpendingInfoDAO()

  override def getBalance(): Future[CurrencyUnit] = listUtxos().map { utxos =>
    utxos.map(_.value).fold(0.bitcoin)(_ + _)
  }

  /** The default HD coin */
  private lazy val DEFAULT_HD_COIN: HDCoin = {
    val coinType = chainParams match {
      case MainNetChainParams                         => HDCoinType.Bitcoin
      case RegTestNetChainParams | TestNetChainParams => HDCoinType.Testnet
    }
    HDCoin(walletConfig.defaultAccountKind, coinType)
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

  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAll()

  /**
    * Tries to convert the provided spk to an address, and then checks if we have
    * it in our address table
    */
  private def findAddress(
      spk: ScriptPubKey): Future[Either[AddUtxoError, AddressDb]] =
    BitcoinAddress.fromScriptPubKey(spk, networkParameters) match {
      case Success(address) =>
        addressDAO.findAddress(address).map {
          case Some(addrDb) => Right(addrDb)
          case None         => Left(AddressNotFound)
        }
      case Failure(_) => Future.successful(Left(BadSPK))
    }

  /** Constructs a DB level representation of the given UTXO, and persist it to disk */
  private def writeUtxo(
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb): Future[UTXOSpendingInfoDb] = {

    val utxo: UTXOSpendingInfoDb = addressDb match {
      case segwitAddr: SegWitAddressDb =>
        NativeV0UTXOSpendingInfoDb(
          id = None,
          outPoint = outPoint,
          output = output,
          privKeyPath = segwitAddr.path,
          scriptWitness = segwitAddr.witnessScript
        )
      case LegacyAddressDb(path, _, _, _) =>
        LegacyUTXOSpendingInfoDb(id = None,
                                 outPoint = outPoint,
                                 output = output,
                                 privKeyPath = path)
      case nested: NestedSegWitAddressDb =>
        throw new IllegalArgumentException(
          s"Bad utxo $nested. Note: nested segwit is not implemented")
    }

    utxoDAO.create(utxo).map { written =>
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txId.hex}:${writtenOut.vout.toInt} into DB")
      logger.info(s"UTXO details: ${written.output}")
      written
    }
  }

  /**
    * @inheritdoc
    */
  override def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[AddUtxoResult] = {
    import AddUtxoError._
    import org.bitcoins.core.util.EitherUtil.EitherOps._

    logger.info(s"Adding UTXO to wallet: ${transaction.txId.hex}:${vout.toInt}")

    // first check: does the provided vout exist in the tx?
    val voutIndexOutOfBounds: Boolean = {
      val voutLength = transaction.outputs.length
      val outOfBunds = voutLength <= vout.toInt

      if (outOfBunds)
        logger.error(
          s"TX with TXID ${transaction.txId.hex} only has $voutLength, got request to add vout ${vout.toInt}!")
      outOfBunds
    }

    if (voutIndexOutOfBounds) {
      Future.successful(VoutIndexOutOfBounds)
    } else {

      val output = transaction.outputs(vout.toInt)
      val outPoint = TransactionOutPoint(transaction.txId, vout)

      // second check: do we have an address associated with the provided
      // output in our DB?
      val addressDbEitherF: Future[Either[AddUtxoError, AddressDb]] =
        findAddress(output.scriptPubKey)

      // insert the UTXO into the DB
      addressDbEitherF.flatMap { addressDbE =>
        val biasedE: Either[AddUtxoError, Future[UTXOSpendingInfoDb]] = for {
          addressDb <- addressDbE
        } yield writeUtxo(output, outPoint, addressDb)

        EitherUtil.liftRightBiasedFutureE(biasedE)
      } map {
        case Right(_) => AddUtxoSuccess(this)
        case Left(e)  => e
      }
    }
  }

  /**
    * @inheritdoc
    */
  // override def updateUtxo: Future[WalletApi] = ???

  override def listUtxos(): Future[Vector[UTXOSpendingInfoDb]] =
    utxoDAO.findAllUTXOs()

  /**
    * @param account Account to generate address from
    * @param chainType What chain do we generate from? Internal change vs. external
    */
  private def getNewAddressHelper(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress] = {

    val accountIndex = account.hdAccount.index

    val lastAddrOptF = chainType match {
      case HDChainType.External =>
        addressDAO.findMostRecentExternal(accountIndex)
      case HDChainType.Change =>
        addressDAO.findMostRecentChange(accountIndex)
    }

    lastAddrOptF.flatMap { lastAddrOpt =>
      val addrPath: HDPath = lastAddrOpt match {
        case Some(addr) =>
          addr.path.next
        case None =>
          val account = HDAccount(DEFAULT_HD_COIN, accountIndex)
          val chain = account.toChain(chainType)
          val address = HDAddress(chain, 0)
          address.toPath
      }

      val addressDb = {
        val pathDiff =
          account.hdAccount.diff(addrPath) match {
            case Some(value) => value
            case None =>
              throw new RuntimeException(
                s"Could not diff ${account.hdAccount} and $addrPath")
          }

        val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
          case Failure(exception) => throw exception
          case Success(value)     => value.key
        }

        addrPath match {
          case segwitPath: SegWitHDPath =>
            AddressDbHelper
              .getSegwitAddress(pubkey, segwitPath, networkParameters)
          case legacyPath: LegacyHDPath =>
            AddressDbHelper.getLegacyAddress(pubkey,
                                             legacyPath,
                                             networkParameters)
          case nestedPath: NestedSegWitHDPath =>
            AddressDbHelper.getNestedSegwitAddress(pubkey,
                                                   nestedPath,
                                                   networkParameters)
        }
      }
      val writeF = addressDAO.create(addressDb)
      writeF.foreach { written =>
        logger.info(
          s"Got ${chainType} address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}")
      }

      writeF.map(_.address)
    }
  }

  /**
    * right now only generates P2WPKH addresses
    *
    * @inheritdoc
    */
  override def getNewAddress(account: AccountDb): Future[BitcoinAddress] = {
    getNewAddressHelper(account, HDChainType.External)
  }

  /** Generates a new change address */
  override protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = {
    getNewAddressHelper(account, HDChainType.Change)
  }

  /** @inheritdoc */
  override protected[wallet] def getDefaultAccount(): Future[AccountDb] = {
    for {
      account <- accountDAO.read((DEFAULT_HD_COIN, 0))
    } yield
      account.getOrElse(
        throw new RuntimeException(
          s"Could not find account with ${DEFAULT_HD_COIN.purpose.constant} " +
            s"purpose field and ${DEFAULT_HD_COIN.coinType.toInt} coin field"))
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
