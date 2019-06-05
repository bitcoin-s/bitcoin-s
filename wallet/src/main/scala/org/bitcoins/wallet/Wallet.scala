package org.bitcoins.wallet

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.{BitcoinSLogger, EitherUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models._
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import org.bitcoins.core.hd._
import org.bitcoins.wallet.config.WalletAppConfig

sealed abstract class Wallet
    extends LockedWallet
    with UnlockedWalletApi
    with BitcoinSLogger {

  /**
    * @inheritdoc
    */
  override def lock(): LockedWalletApi = {
    logger.debug(s"Locking wallet")
    val encryptedT = EncryptedMnemonicHelper.encrypt(mnemonicCode, passphrase)
    val encrypted = encryptedT match {
      case Failure(exception) =>
        throw new RuntimeException(s"Could not encrypt mnemonic: $exception")
      case Success(value) => value
    }

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
        val destinations: Seq[TransactionOutput] = List(
          TransactionOutput(amount, address.scriptPubKey))

        // currencly just grabs one utxos, throws if can't find big enough
        // todo: implement coin selection
        val utxos: List[BitcoinUTXOSpendingInfo] =
          List(
            walletUtxos
              .find(_.value >= amount)
              .get
              .toUTXOSpendingInfo(fromAccount, seed))

        logger.info(s"Spending UTXOs: ${utxos
          .map { utxo =>
            import utxo.outPoint
            s"${outPoint.txId.hex}:${outPoint.vout.toInt}"
          }
          .mkString(", ")}")

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
      /* todo: add change output to UTXO DB
       _ <- {
        val changeVout = ???
        addUtxo(signed, changeVout)
      } */
    } yield {
      signed
    }
  }

}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends CreateWalletApi with BitcoinSLogger {

  // The default HD purpose of the bitcoin-s wallet. Can be
  // one of segwit, nested segwit or legacy. Hard coded for
  // now, could be make configurable in the future
  private[wallet] val DEFAULT_HD_PURPOSE: HDPurpose = HDPurposes.SegWit

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
  val badPassphrase = AesPassword("changeMe")

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
      mnemonicE.flatMap { mnemonic =>
        val encryptedT = EncryptedMnemonicHelper
          .encrypt(mnemonic, badPassphrase)

        val encryptedE: Either[Throwable, EncryptedMnemonic] =
          encryptedT match {
            case Failure(exception) => Left(exception)
            case Success(value)     => Right(value)
          }

        encryptedE.left
          .map { err =>
            logger.error(s"Encryption error when encrypting mnemonic: $err")
            InitializeWalletError.EncryptionError(err)
          }
      }

    val biasedFinalEither: Either[InitializeWalletError, Future[WalletImpl]] =
      for {
        mnemonic <- mnemonicE
        encrypted <- encryptedMnemonicE
      } yield {
        val wallet = WalletImpl(mnemonic)
        val coin =
          HDCoin(DEFAULT_HD_PURPOSE, HDUtil.getCoinType(config.network))
        val account = HDAccount(coin, 0)
        val xpriv = wallet.xprivForPurpose(DEFAULT_HD_PURPOSE)

        // safe since we're deriving from a priv
        val xpub = xpriv.deriveChildPubKey(account).get
        val accountDb = AccountDb(xpub, account)

        val mnemonicPath =
          WalletStorage.writeMnemonicToDisk(encrypted)
        logger.debug(s"Saved encrypted wallet mnemonic to $mnemonicPath")

        for {
          _ <- config.initialize()
          _ <- wallet.accountDAO
            .create(accountDb)
            .map(_ => logger.trace(s"Saved account to DB"))
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
}
