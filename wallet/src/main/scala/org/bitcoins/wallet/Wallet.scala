package org.bitcoins.wallet
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto.bip44._
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint, TransactionOutput}
import org.bitcoins.core.util.{BitcoinSLogger, EitherUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.wallet.api.AddUtxoError.{AddressNotFound, BadSPK}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

sealed abstract class Wallet extends UnlockedWalletApi with BitcoinSLogger {
  implicit def ec: ExecutionContext

  val addressDAO: AddressDAO = AddressDAO(dbConfig)
  val mnemonicDAO: MnemonicCodeDAO = MnemonicCodeDAO(dbConfig)
  val accountDAO: AccountDAO = AccountDAO(dbConfig)
  val utxoDAO: UTXOSpendingInfoDAO = UTXOSpendingInfoDAO(dbConfig)

  val bip44Coin: BIP44Coin = BIP44Coin.fromChainParams(chainParams)


  /**
    * @inheritdoc
    */
  override def lock: Future[LockedWalletApi] = ???

  override def getBalance(): Future[CurrencyUnit] = listUtxos().map { utxos =>
    utxos.map(_.value).fold(Bitcoins.zero)(_ + _)
  }

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

  private def writeUtxo(
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb): Future[UTXOSpendingInfoDb] = {

    val utxo = UTXOSpendingInfoDb(
      id = None,
      outPoint = outPoint,
      output = output,
      privKeyPath = addressDb.path,
      redeemScriptOpt = None, // todo fix me when implementing P2SH addresses
      scriptWitnessOpt = addressDb.witnessScriptOpt
    )

    utxoDAO.create(utxo).map { _ =>
      logger.trace(
        s"Successfully inserted UTXO ${outPoint.txId.hex}:${outPoint.vout.toInt} into DB")
      utxo
    }
  }

  /**
    * @inheritdoc
    */
  override def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[AddUtxoResult] = {
    import AddUtxoError._

    logger.trace(
      s"Adding UTXO to wallet. TXID: ${transaction.txId.hex}, vout: ${vout.toInt}")

    // first check: does the provided vout exist in the tx?
    val voutIndexOutOfBounds: Boolean = {
      val voutLength = transaction.outputs.length
      val outOfBunds = voutLength <= vout.toInt

      if (outOfBunds)
        logger.error(
          s"TX with TXID ${transaction.txId.hex} only has $voutLength, got request to add vout ${vout.toInt}!")
      outOfBunds
    }

    val outputE: Either[AddUtxoError, TransactionOutput] =
      if (voutIndexOutOfBounds) Left(VoutIndexOutOfBounds)
      else
        Right(transaction.outputs(vout.toInt))

    val outPointE: Either[AddUtxoError, TransactionOutPoint] =
      if (voutIndexOutOfBounds) Left(VoutIndexOutOfBounds)
      else Right(TransactionOutPoint(transaction.txId, vout))

    // second check: do we have an address associated with the provided
    // output in our DB?
    val addressDbEitherF: Future[Either[AddUtxoError, AddressDb]] = {
      val nested = outputE.map(out => findAddress(out.scriptPubKey))
      EitherUtil.flattenFutureE(nested)
    }

    // insert the UTXO into the DB
    addressDbEitherF.flatMap { addressDbE =>
      val biasedE: Either[AddUtxoError, Future[UTXOSpendingInfoDb]] = for {
        output <- outputE
        outPoint <- outPointE
        addressDb <- addressDbE
      } yield writeUtxo(output, outPoint, addressDb)

      EitherUtil.liftRightBiasedFutureE(biasedE)

    } map {
      case Right(_) => AddUtxoSuccess(this)
      case Left(e)  => e
    }
  }

  /**
    * @inheritdoc
    */
  // override def updateUtxo: Future[WalletApi] = ???

  override def listUtxos(): Future[Vector[UTXOSpendingInfoDb]] =
    utxoDAO.findAllUTXOs()

  /**
    * @param accountIndex Account index to generate address from
    * @param chainType What chain do we generate from? Internal change vs. external
    */
  private def getNewAddressHelper(
      accountIndex: Int,
      chainType: BIP44ChainType): Future[BitcoinAddress] = {
    val lastAddrOptF = chainType match {
      case BIP44ChainType.External =>
        addressDAO.findMostRecentExternal(accountIndex)
      case BIP44ChainType.Change =>
        addressDAO.findMostRecentChange(accountIndex)
    }

    lastAddrOptF.flatMap { lastAddrOpt =>
      val addrPath = lastAddrOpt match {
        case Some(addr) => addr.path.next.address
        case None =>
          val account = BIP44Account(bip44Coin, accountIndex)
          val chain = account.toChain(chainType)
          BIP44Address(chain, 0)
      }

      val addressDb =
        AddressDbHelper
          .getP2WPKHAddress(xpriv, addrPath.toPath, networkParameters)
      addressDAO.create(addressDb).map(_.address)
    }
  }

  /**
    * right now only generates P2WPKH addresses
    *
    * @inheritdoc
    */
  override def getNewAddress(accountIndex: Int = 0): Future[BitcoinAddress] = {
    getNewAddressHelper(accountIndex, BIP44ChainType.External)
  }

  /** Generates a new change address */
  private def getNewChangeAddress(
      accountIndex: Int = 0): Future[BitcoinAddress] = {
    getNewAddressHelper(accountIndex, BIP44ChainType.Change)
  }

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      accountIndex: Int = 0): Future[Transaction] = {
    for {
      change <- getNewChangeAddress(accountIndex)
      walletUtxos <- listUtxos()
      txBuilder <- {
        val destinations: Seq[TransactionOutput] = List(
          TransactionOutput(amount, address.scriptPubKey))

        // currencly just grabs one utxos, throws if can't find big enough
        // todo: implement coin selection
        val utxos: List[BitcoinUTXOSpendingInfo] =
          List(walletUtxos.find(_.value >= amount).get.toUTXOSpendingInfo(this))


        val b = networkParameters match {
          case b: BitcoinNetwork =>
            BitcoinTxBuilder(destinations = destinations,
              utxos = utxos,
              feeRate = feeRate,
              changeSPK = change.scriptPubKey,
              network = b)
        }
        
        b
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

  /**
    * @inheritdoc
    */
  override def unlock(passphrase: AesPassword): Future[UnlockWalletResult] = ???

  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  override def listAddresses(): Future[Vector[AddressDb]] = addressDAO.findAll()

  /**
    * @inheritdoc
    */
  // override def createNewAccount: Future[Try[WalletApi]] = ???

}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends CreateWalletApi with BitcoinSLogger {
  private case class WalletImpl(
      mnemonicCode: MnemonicCode,
      walletAppConfig: WalletAppConfig)(implicit val ec: ExecutionContext)
      extends Wallet {

    // todo: until we've figured out a better schem
    override val passphrase: AesPassword = Wallet.badPassphrase
  }

  def apply(
      mnemonicCode: MnemonicCode,
      walletAppConfig: WalletAppConfig)(implicit ec: ExecutionContext): Wallet =
    WalletImpl(mnemonicCode, walletAppConfig)(ec)

  // todo figure out how to handle password part of wallet
  val badPassphrase = AesPassword("changeMe")

  // todo fix signature
  override protected def initializeWithEntropy(
      entropy: BitVector, appConfig: WalletAppConfig)(
      implicit ec: ExecutionContext): Future[InitializeWalletResult] = {

    val chainParams = appConfig.chain

    logger.info(s"Initializing wallet on chain $chainParams")

    val mnemonicT = Try(MnemonicCode.fromEntropy(entropy))
    val mnemonicE: Either[InitializeWalletError, MnemonicCode] =
      mnemonicT match {
        case Success(mnemonic) =>
          logger.info(s"Created mnemonic from entropy")
          Right(mnemonic)
        case Failure(err) =>
          logger.error(s"Could not create mnemonic from entropy! $err")
          Left(InitializeWalletError.BadEntropy)

      }

    val encryptedMnemonicE: Either[InitializeWalletError, EncryptedMnemonic] =
      mnemonicE.flatMap { mnemonic =>
        EncryptedMnemonicHelper
          .encrypt(mnemonic, badPassphrase)
          .toEither
          .left
          .map { err =>
            logger.error(s"Encryption error when encrying mnemonic: $err")
            InitializeWalletError.EncryptionError(err)
          }
      }

    val biasedFinalEither: Either[InitializeWalletError, Future[WalletImpl]] =
      for {
        mnemonic <- mnemonicE
        encrypted <- encryptedMnemonicE
      } yield {
        val wallet = WalletImpl(mnemonic, appConfig)
        val account = BIP44Account(BIP44Coin.fromChainParams(chainParams), 0)
        val xpriv = wallet.xpriv

        // safe since we're deriving from a priv
        val xpub = xpriv.deriveChildPubKey(account).get
        val accountDb = AccountDb(xpub, account)

        for {
          _ <- wallet.mnemonicDAO
            .create(encrypted)
            .map(_ => logger.info(s"Saved encrypted mnemonic to disk"))

          _ <- wallet.accountDAO
            .create(accountDb)
            .map(_ => logger.info(s"Saved account to DB"))
        } yield wallet
      }

    val finalEither: Future[Either[InitializeWalletError, WalletImpl]] =
      EitherUtil.liftRightBiasedFutureE(biasedFinalEither)

    finalEither.map {
      case Right(wallet) =>
        logger.info(s"Successfully initialized wallet")
        InitializeWalletSuccess(wallet)
      case Left(err) => err
    }
  }
}
