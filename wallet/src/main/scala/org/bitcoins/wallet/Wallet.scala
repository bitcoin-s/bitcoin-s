package org.bitcoins.wallet

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.EitherUtil
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import org.bitcoins.db.KeyHandlingLogger

sealed abstract class Wallet extends LockedWallet with UnlockedWalletApi {

  /**
    * @inheritdoc
    */
  override def lock(): LockedWalletApi = {
    logger.debug(s"Locking wallet")
    val encrypted = EncryptedMnemonicHelper.encrypt(mnemonicCode, passphrase)

    WalletStorage.writeMnemonicToDisk(encrypted)
    logger.debug("Locked wallet")
    LockedWallet()
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
            .map(_.toUTXOSpendingInfo(fromAccount, seed))

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
      _ <- processOurTransaction(signed, confirmations = 0)
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

  override def createNewAccount(): Future[WalletApi] =
    createNewAccount(DEFAULT_HD_COIN.purpose)

  // todo: check if there's addresses in the most recent
  // account before creating new
  override def createNewAccount(purpose: HDPurpose): Future[Wallet] = {

    accountDAO
      .findAll()
      .map(_.filter(_.hdAccount.purpose == purpose))
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
          s"Creating new account at index $accountIndex for purpose $purpose")
        val newAccount = HDAccount(DEFAULT_HD_COIN, accountIndex)
        val xpub = {
          val xpriv = xprivForPurpose(newAccount.purpose)
          xpriv.deriveChildPubKey(newAccount) match {
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
      .map(_ => this)
  }
}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends CreateWalletApi with KeyHandlingLogger {

  private case class WalletImpl(
      mnemonicCode: MnemonicCode
  )(
      implicit override val walletConfig: WalletAppConfig,
      override val ec: ExecutionContext)
      extends Wallet {

    // todo: until we've figured out a better schem
    override val passphrase: AesPassword = Wallet.badPassphrase
  }

  def apply(mnemonicCode: MnemonicCode)(
      implicit config: WalletAppConfig,
      ec: ExecutionContext): Wallet =
    WalletImpl(mnemonicCode)

  // todo figure out how to handle password part of wallet
  val badPassphrase = AesPassword.fromNonEmptyString("changeMe")

  // todo fix signature
  override def initializeWithEntropy(entropy: BitVector)(
      implicit config: WalletAppConfig,
      ec: ExecutionContext): Future[InitializeWalletResult] = {
    import org.bitcoins.core.util.EitherUtil.EitherOps._

    logger.info(s"Initializing wallet on chain ${config.network}")

    val mnemonicT = Try(MnemonicCode.fromEntropy(entropy))
    val mnemonicE: Either[InitializeWalletError, MnemonicCode] =
      mnemonicT match {
        case Success(mnemonic) =>
          logger.trace(s"Created mnemonic from entropy")
          Right(mnemonic)
        case Failure(err) =>
          logger.error(s"Could not create mnemonic from entropy! $err")
          Left(InitializeWalletError.BadEntropy)

      }

    val encryptedMnemonicE: Either[InitializeWalletError, EncryptedMnemonic] =
      mnemonicE.map { EncryptedMnemonicHelper.encrypt(_, badPassphrase) }

    val biasedFinalEither: Either[InitializeWalletError, Future[WalletImpl]] =
      for {
        mnemonic <- mnemonicE
        encrypted <- encryptedMnemonicE
      } yield {
        val wallet = WalletImpl(mnemonic)

        for {
          _ <- config.initialize()
          _ = {
            val mnemonicPath =
              WalletStorage.writeMnemonicToDisk(encrypted)
            logger.debug(s"Saved encrypted wallet mnemonic to $mnemonicPath")
          }
          _ <- {
            // We want to make sure all level 0 accounts are created,
            // so the user can change the default account kind later
            // and still have their wallet work
            val createAccountFutures =
              HDPurposes.all.map(createRootAccount(wallet, _))

            val accountCreationF = Future.sequence(createAccountFutures)

            accountCreationF.foreach(_ =>
              logger.debug(s"Created root level accounts for wallet"))

            accountCreationF.failed.foreach { err =>
              logger.error(s"Failed to create root level accounts: $err")
            }

            accountCreationF
          }

        } yield wallet
      }

    val finalEither: Future[Either[InitializeWalletError, WalletImpl]] =
      EitherUtil.liftRightBiasedFutureE(biasedFinalEither)

    finalEither.map {
      case Right(wallet) =>
        logger.debug(s"Successfully initialized wallet")
        InitializeWalletSuccess(wallet)
      case Left(err) => err
    }
  }

  /** Creates the level 0 account for the given HD purpose */
  private def createRootAccount(wallet: Wallet, purpose: HDPurpose)(
      implicit config: WalletAppConfig,
      ec: ExecutionContext): Future[AccountDb] = {

    val coin =
      HDCoin(purpose, HDUtil.getCoinType(config.network))
    val account = HDAccount(coin, 0)
    val xpriv = wallet.xprivForPurpose(purpose)
    // safe since we're deriving from a priv
    val xpub = xpriv.deriveChildPubKey(account).get
    val accountDb = AccountDb(xpub, account)

    logger.debug(s"Creating account with constant prefix $purpose")
    wallet.accountDAO
      .create(accountDb)
      .map { written =>
        logger.debug(s"Saved account with constant prefix $purpose to DB")
        written
      }

  }
}
