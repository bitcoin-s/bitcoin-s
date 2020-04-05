package org.bitcoins.wallet

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.keymanager.KeyManagerParams
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.keymanager.util.HDUtil
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

sealed abstract class Wallet extends PTLCWallet with UnlockedWalletApi {

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi

  /**
    * @inheritdoc
    */
  override def lock(): LockedWalletApi = {
    logger.debug(s"Locking wallet")
    LockedWallet(nodeApi, chainQueryApi)
  }

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb): Future[Transaction] = {
    logger.info(s"Sending $amount to $address at feerate $feeRate")
    val destination = TransactionOutput(amount, address.scriptPubKey)
    for {
      txBuilder <- fundRawTransactionInternal(
        destinations = Vector(destination),
        feeRate = feeRate,
        fromAccount = fromAccount,
        keyManagerOpt = Some(keyManager),
        markAsReserved = false)
      signed <- txBuilder.sign
      ourOuts <- findOurOuts(signed)
      _ <- processOurTransaction(transaction = signed,
                                 feeRate = feeRate,
                                 inputAmount = txBuilder.creditingAmount,
                                 sentAmount = txBuilder.destinationAmount,
                                 blockHashOpt = None)
    } yield {
      logger.debug(
        s"Signed transaction=${signed.txIdBE.hex} with outputs=${signed.outputs.length}, inputs=${signed.inputs.length}")

      logger.trace(s"Change output(s) for transaction=${signed.txIdBE.hex}")
      ourOuts.foreach { out =>
        logger.trace(s"    $out")
      }
      signed
    }
  }

  /** Creates a new account my reading from our account database, finding the last account,
    * and then incrementing the account index by one, and then creating that account
    *
    * @param kmParams
    * @return
    */
  override def createNewAccount(kmParams: KeyManagerParams): Future[Wallet] = {
    val lastAccountOptF = accountDAO
      .findAll()
      .map(_.filter(_.hdAccount.purpose == kmParams.purpose))
      .map(_.sortBy(_.hdAccount.index))
      // we want to the most recently created account,
      // to know what the index of our new account
      // should be.
      .map(_.lastOption)

    lastAccountOptF.flatMap {
      case Some(accountDb) =>
        val hdAccount = accountDb.hdAccount
        val newAccount = hdAccount.copy(index = hdAccount.index + 1)
        createNewAccount(newAccount, kmParams)
      case None =>
        createNewAccount(walletConfig.defaultAccount, kmParams)
    }
  }

  // todo: check if there's addresses in the most recent
  // account before creating new
  override def createNewAccount(
      hdAccount: HDAccount,
      kmParams: KeyManagerParams): Future[Wallet] = {
    val accountIndex = hdAccount.index
    logger.info(
      s"Creating new account at index $accountIndex for purpose ${kmParams.purpose}")
    val hdCoin =
      HDCoin(purpose = keyManager.kmParams.purpose,
             coinType = HDUtil.getCoinType(keyManager.kmParams.network))
    val newAccount = HDAccount(hdCoin, accountIndex)
    val xpub: ExtPublicKey = {
      keyManager.deriveXPub(newAccount) match {
        case Failure(exception) =>
          // this won't happen, because we're deriving from a privkey
          // this should really be changed in the method signature
          logger.error(s"Unexpected error when deriving xpub: $exception")
          throw exception
        case Success(xpub) => xpub
      }
    }
    val newAccountDb = AccountDb(xpub, newAccount)
    val accountCreationF = accountDAO.create(newAccountDb)
    accountCreationF.map(created =>
      logger.debug(s"Created new account ${created.hdAccount}"))
    accountCreationF
      .map(_ => Wallet(keyManager, nodeApi, chainQueryApi))
  }
}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends WalletLogger {

  private case class WalletImpl(
      override val keyManager: BIP39KeyManager,
      override val nodeApi: NodeApi,
      override val chainQueryApi: ChainQueryApi
  )(
      implicit override val walletConfig: WalletAppConfig,
      override val ec: ExecutionContext
  ) extends Wallet

  def apply(
      keyManager: BIP39KeyManager,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit config: WalletAppConfig,
      ec: ExecutionContext): Wallet = {
    WalletImpl(keyManager, nodeApi, chainQueryApi)
  }

  /** Creates the level 0 account for the given HD purpose */
  private def createRootAccount(wallet: Wallet, keyManager: BIP39KeyManager)(
      implicit walletAppConfig: WalletAppConfig,
      ec: ExecutionContext): Future[AccountDb] = {
    val coinType = HDUtil.getCoinType(keyManager.kmParams.network)
    val coin =
      HDCoin(purpose = keyManager.kmParams.purpose, coinType = coinType)
    val account = HDAccount(coin = coin, index = 0)
    // safe since we're deriving from a priv
    val xpub = keyManager.deriveXPub(account).get
    val accountDb = AccountDb(xpub, account)
    logger.debug(
      s"Creating account with constant prefix ${keyManager.kmParams.purpose}")
    wallet.accountDAO
      .create(accountDb)
      .map { written =>
        logger.debug(
          s"Saved account with constant prefix ${keyManager.kmParams.purpose} to DB")
        written
      }
  }

  def initialize(wallet: Wallet, bip39PasswordOpt: Option[String])(
      implicit walletAppConfig: WalletAppConfig,
      ec: ExecutionContext): Future[Wallet] = {
    // We want to make sure all level 0 accounts are created,
    // so the user can change the default account kind later
    // and still have their wallet work
    val initConfigF = walletAppConfig.initialize()
    val createAccountFutures = for {
      _ <- initConfigF
      accounts = HDPurposes.all.map { purpose =>
        //we need to create key manager params for each purpose
        //and then initialize a key manager to derive the correct xpub
        val kmParams = wallet.keyManager.kmParams.copy(purpose = purpose)
        val kmE = {
          BIP39KeyManager.fromParams(kmParams = kmParams,
                                     password = BIP39KeyManager.badPassphrase,
                                     bip39PasswordOpt = bip39PasswordOpt)
        }
        kmE match {
          case Right(km) => createRootAccount(wallet = wallet, keyManager = km)
          case Left(err) =>
            //probably means you haven't initialized the key manager via the
            //'CreateKeyManagerApi'
            throw new RuntimeException(
              s"Failed to create keymanager with params=${kmParams} err=${err}")
        }

      }
    } yield accounts

    val accountCreationF =
      createAccountFutures.flatMap(accounts => Future.sequence(accounts))

    accountCreationF.foreach(_ =>
      logger.debug(s"Created root level accounts for wallet"))

    accountCreationF.failed.foreach { err =>
      logger.error(s"Failed to create root level accounts: $err")
    }

    accountCreationF.map(_ => wallet)
  }

}
