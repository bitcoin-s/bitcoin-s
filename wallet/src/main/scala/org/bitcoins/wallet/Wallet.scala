package org.bitcoins.wallet

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.keymanager.util.HDUtil
import org.bitcoins.keymanager.{KeyManager, KeyManagerParams}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

sealed abstract class Wallet extends LockedWallet with UnlockedWalletApi {

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
    for {
      change <- getNewChangeAddress(fromAccount)
      walletUtxos <- listUtxos()
      txBuilder <- {
        val destinations = Vector(
          TransactionOutput(amount, address.scriptPubKey))

        // currencly just grabs the biggest utxos until it finds enough
        val utxos: Vector[BitcoinUTXOSpendingInfo] =
          CoinSelector
            .accumulateLargest(walletUtxos, destinations, feeRate)
            .map(
              _.toUTXOSpendingInfo(account = fromAccount,
                                   keyManager = keyManager,
                                   network = networkParameters))

        logger.info({
          val utxosStr = utxos
            .map { utxo =>
              import utxo.outPoint
              s"${outPoint.txId.hex}:${outPoint.vout.toInt}"
            }
            .mkString(", ")
          s"Spending UTXOs: $utxosStr"
        })

        utxos.zipWithIndex.foreach {
          case (utxo, index) =>
            logger.info(s"UTXO $index details: ${utxo.output}")
        }

        networkParameters match {
          case b: BitcoinNetwork =>
            BitcoinTxBuilder(destinations = destinations,
                             utxos = utxos,
                             feeRate = feeRate,
                             changeSPK = change.scriptPubKey,
                             network = b)
        }

      }
      signed <- txBuilder.sign
      ourOuts <- findOurOuts(signed)
      // TODO internal
      _ <- processOurTransaction(signed, blockHashOpt = None)
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

  // todo: check if there's addresses in the most recent
  // account before creating new
  override def createNewAccount(kmParams: KeyManagerParams): Future[Wallet] = {
    accountDAO
      .findAll()
      .map(_.filter(_.hdAccount.purpose == kmParams.purpose))
      .map(_.sortBy(_.hdAccount.index))
      // we want to the most recently created account,
      // to know what the index of our new account
      // should be.
      .map(_.lastOption)
      .flatMap { mostRecentOpt =>
        val accountIndex = mostRecentOpt match {
          case None          => 0 // no accounts present in wallet
          case Some(account) => account.hdAccount.index + 1
        }
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
      }
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

  def initialize(wallet: Wallet)(
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
        val kmE =
          BIP39KeyManager.fromParams(kmParams, BIP39KeyManager.badPassphrase)
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
