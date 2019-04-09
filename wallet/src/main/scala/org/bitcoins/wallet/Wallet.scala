package org.bitcoins.wallet
import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto.bip44._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{
  Address,
  Bech32Address,
  BitcoinAddress,
  P2PKHAddress
}
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.script.{
  P2PKHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.{BitcoinSLogger, EitherUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.UTXOSpendingInfo
import org.bitcoins.db.DbConfig
import org.bitcoins.wallet.api.{
  CreateWalletApi,
  FeeProvider,
  UnlockedWalletApi,
  WalletApi
}
import org.bitcoins.wallet.models._
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

sealed abstract class Wallet extends UnlockedWalletApi with BitcoinSLogger {
  implicit def ec: ExecutionContext

  val addressDAO: AddressDAO = AddressDAO(dbConfig)
  val mnemonicDAO: MnemonicCodeDAO = MnemonicCodeDAO(dbConfig)
  val accountDAO: AccountDAO = AccountDAO(dbConfig)
  val utxoDAO: UTXOSpendingInfoDAO = UTXOSpendingInfoDAO(dbConfig)

  val bip44Coin: BIP44Coin = BIP44Coin.fromChainParams(chainParams)

  def dbConfig: DbConfig

  def chainParams: ChainParams

  val feeProvider: FeeProvider = _ =>
    Future.successful(SatoshisPerByte(Satoshis.one))

  /**
    * @inheritdoc
    */
  override def lock: Future[WalletApi] = ???

  /**
    * @inheritdoc
    */
  def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[Either[AddUtxoError, WalletApi]] = {
    val voutIndexOutOfBounds = transaction.outputs.length >= vout.toInt

    val outputE: Either[AddUtxoError, TransactionOutput] =
      if (voutIndexOutOfBounds) Left(VoutIndexOutOfBounds)
      else
        Right(transaction.outputs(vout.toInt))

    val outPointE: Either[AddUtxoError, TransactionOutPoint] =
      if (voutIndexOutOfBounds) Left(VoutIndexOutOfBounds)
      else Right(TransactionOutPoint(transaction.txId, vout))

    val addressEitherF: Future[Either[AddUtxoError, AddressDb]] = {
      val nestedAddressE = for {
        output <- outputE
        address <- BitcoinAddress
          .fromScriptPubKey(output.scriptPubKey, networkParameters)
          .toEither
          .left
          .map(_ => BadSPK)
      } yield
        addressDAO.findAddress(address).map {
          case Some(addr) => Right(addr)
          case None       => Left(AddressNotFound)
        }
      EitherUtil.flattenFutureE(nestedAddressE)
    }

    addressEitherF.flatMap { addressE =>
      val nestedWalletE: Either[
        AddUtxoError,
        Future[Either[AddUtxoError, WalletApi]]] =
        for {
          output <- outputE
          outPoint <- outPointE
          address <- addressE
        } yield {
          val utxo =
            UTXOSpendingInfoDb(None, outPoint, output, address.path, None, None)
          utxoDAO.create(utxo).map(_ => Right(this.asInstanceOf[WalletApi]))
        }
      EitherUtil.flattenFutureE(nestedWalletE)
    }
  }

  sealed abstract class AddUtxoError
  case object VoutIndexOutOfBounds extends AddUtxoError
  case object BadSPK extends AddUtxoError
  case object AddressNotFound extends AddUtxoError

  /**
    * @inheritdoc
    */
  override def updateUtxo: Future[WalletApi] = ???

  /**
    * right now only generates P2WPKH addresses
    *
    * @inheritdoc
    */
  override def getNewAddress(accountIndex: Int = 0): Future[Address] = {
    addressDAO.findMostRecentExternal(accountIndex).flatMap { addrOpt =>
      val addrPath = addrOpt match {
        case Some(addr) => addr.path.next.address
        case None => {
          val account = BIP44Account(bip44Coin, accountIndex)
          val chain = BIP44Chain(BIP44ChainType.External, account)
          BIP44Address(chain, 0)
        }
      }
      val addressDb =
        AddressDbHelper
          .getP2WPKHAddress(xpriv, addrPath.toPath, networkParameters)
      addressDAO.create(addressDb).map(_.address)
    }
  }

  /**
    * @inheritdoc
    */
  override def unlock(passphrase: String): Future[Try[UnlockedWalletApi]] = ???

  /**
    * @inheritdoc
    */
  override def getAccounts: Future[Vector[BIP44Account]] = ???

  /**
    * @inheritdoc
    */
  override def createNewAccount: Future[Try[WalletApi]] = ???

}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends CreateWalletApi {
  private case class WalletImpl(
      mnemonicCode: MnemonicCode,
      passphrase: String,
      dbConfig: DbConfig,
      chainParams: ChainParams)(implicit executionContext: ExecutionContext)
      extends Wallet {
    override implicit def ec: ExecutionContext = executionContext

    /**
      * Adds the provided UTXO to the wallet, making it
      * available for spending.
      */
    override def addUtxo(utxo: UTXOSpendingInfo): Future[WalletApi] = ???
  }

  def apply(
      mnemonicCode: MnemonicCode,
      passphrase: String = BIP39Seed.EMPTY_PASSWORD,
      dbConfig: DbConfig,
      chainParams: ChainParams)(implicit ec: ExecutionContext): Wallet =
    WalletImpl(mnemonicCode, passphrase, dbConfig, chainParams)(ec)

  override protected def initializeWithEntropy(
      entropy: BitVector,
      chainParams: ChainParams,
      passphrase: String = BIP39Seed.EMPTY_PASSWORD,
      dbConfig: Option[DbConfig])(
      implicit ec: ExecutionContext): Future[UnlockedWalletApi] = {
    val actualDbConf =
      dbConfig.getOrElse(DbConfig.fromChainParams(chainParams))

    val mnemonic = MnemonicCode.fromEntropy(entropy)
    val encryptedMnemonic =
      EncryptedMnemonicHelper.encrypt(mnemonic, passphrase)

    val wallet = WalletImpl(mnemonic, passphrase, actualDbConf, chainParams)

    val account = BIP44Account(BIP44Coin.fromChainParams(chainParams), 0)

    val xpriv = wallet.xpriv

    // safe since we're deriving from a priv
    val xpub = xpriv.deriveChildPubKey(account).get

    val accountDb = AccountDb(xpub, account)

    for {
      _ <- wallet.mnemonicDAO.create(encryptedMnemonic)
      _ <- wallet.accountDAO.create(accountDb)
    } yield wallet
  }
}
