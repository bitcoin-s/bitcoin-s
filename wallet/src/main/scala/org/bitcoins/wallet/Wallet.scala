package org.bitcoins.wallet
import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto.bip44._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
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

  val addressDAO: AddressDAO = AddressDAO(dbConfig, chainParams)
  val mnemonicDAO: MnemonicCodeDAO = MnemonicCodeDAO(dbConfig)
  val accountDAO: AccountDAO = AccountDAO(dbConfig, chainParams)

  def dbConfig: DbConfig

  def chainParams: ChainParams

  val feeProvider: FeeProvider = _ =>
    Future.successful(SatoshisPerByte(Satoshis.one))

  override def mnemonicCode: Future[MnemonicCode] = ???

  /**
    * @inheritdoc
    */
  override def lock: Future[WalletApi] = ???

  /**
    * @inheritdoc
    */
  override def addUtxo(utxo: UTXOSpendingInfo): Future[WalletApi] = ???

  /**
    * @inheritdoc
    */
  override def updateUtxo: Future[WalletApi] = ???

  /**
    * @inheritdoc
    */
  def getNewAddress(accountIndex: Int = 0): Future[Address] = {
    addressDAO.findMostRecentExternal(accountIndex).flatMap { addrOpt =>
      val addrIndex = addrOpt match {
        case Some(addr) => addr.path.next.address.index
        case None       => 0
      }
      ???
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

  /**
    * @inheritdoc
    */
  override def getNewAddress: Future[Address] = ???
}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends CreateWalletApi {
  private case class WalletImpl(dbConfig: DbConfig, chainParams: ChainParams)(
      implicit executionContext: ExecutionContext)
      extends Wallet {
    override implicit def ec: ExecutionContext = executionContext
  }

  def apply(dbConfig: DbConfig, chainParams: ChainParams)(
      implicit ec: ExecutionContext): Wallet =
    WalletImpl(dbConfig, chainParams)(ec)

  override protected def initializeWithEntropy(
      entropy: BitVector,
      chainParams: ChainParams,
      passphrase: String,
      dbConfig: Option[DbConfig])(
      implicit ec: ExecutionContext): Future[WalletApi] = {
    val actualDbConf =
      dbConfig.getOrElse(DbConfig.fromChainParams(chainParams))

    val wallet = WalletImpl(actualDbConf, chainParams)

    val mnemonic = MnemonicCode.fromEntropy(entropy)
    val encryptedMnemonic =
      EncryptedMnemonicHelper.encrypt(mnemonic, passphrase)

    val account = BIP44Account(BIP44Coin.fromChainParams(chainParams), 0)

    val seed = BIP39Seed.fromMnemonic(mnemonic)
    val keyVersion = ExtKeyPrivVersion.fromChainParams(chainParams)
    val xpriv = seed.toExtPrivateKey(keyVersion)

    // safe since we're deriving from a priv
    val xpub = xpriv.deriveChildPubKey(account).get

    val accountDb = AccountDb(xpub, account)

    for {
      _ <- wallet.mnemonicDAO.create(encryptedMnemonic)
      _ <- wallet.accountDAO.create(accountDb)
    } yield WalletImpl(actualDbConf, chainParams)
  }
}
