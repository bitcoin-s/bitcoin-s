package org.bitcoins.wallet

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
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
import org.bitcoins.core.hd.{
  HDAccount,
  HDAddress,
  HDChainType,
  HDCoin,
  HDCoinType,
  HDPath,
  HDPurpose,
  HDPurposes,
  SegWitHDPath
}
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.core.protocol.blockchain.TestNetChainParams

sealed abstract class Wallet extends UnlockedWalletApi with BitcoinSLogger {

  val addressDAO: AddressDAO = AddressDAO(dbConfig)
  val mnemonicDAO: MnemonicCodeDAO = MnemonicCodeDAO(dbConfig)
  val accountDAO: AccountDAO = AccountDAO(dbConfig)
  val utxoDAO: UTXOSpendingInfoDAO = UTXOSpendingInfoDAO(dbConfig)

  /** The default HD coin */
  lazy private val DEFAULT_HD_COIN: HDCoin = {
    val coinType = chainParams match {
      case MainNetChainParams                         => HDCoinType.Bitcoin
      case RegTestNetChainParams | TestNetChainParams => HDCoinType.Testnet
    }
    HDCoin(Wallet.DEFAULT_HD_PURPOSE, coinType)
  }

  /**
    * @inheritdoc
    */
  override def lock: Future[LockedWalletApi] = ???

  override def getBalance(): Future[CurrencyUnit] = listUtxos().map { utxos =>
    utxos.map(_.value).fold(0.bitcoin)(_ + _)
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

    val utxo: UTXOSpendingInfoDb = addressDb match {
      case segwitAddr: SegWitAddressDb =>
        SegWitUTOXSpendingInfodb(
          id = None,
          outPoint = outPoint,
          output = output,
          privKeyPath = segwitAddr.path,
          scriptWitness = segwitAddr.witnessScript
        )
      case otherAddr @ (_: LegacyAddressDb | _: NestedSegWitAddressDb) =>
        throw new IllegalArgumentException(
          s"Bad utxo $otherAddr. Note: Only Segwit is implemented")
    }

    utxoDAO.create(utxo).map { written =>
      import written.{outPoint => writtenOut}
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txId.hex}:${writtenOut.vout.toInt} into DB")
      logger.info(s"UTXO details: ${written.output}")
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

      val addressDb =
        addrPath match {
          case segwitPath: SegWitHDPath =>
            val pathDiff = account.hdAccount.diff(segwitPath) match {
              case Some(value) => value
              case None =>
                throw new RuntimeException(
                  s"Could not diff ${account.hdAccount} and $segwitPath")
            }

            val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
              case Failure(exception) => throw exception
              case Success(value)     => value.key
            }

            AddressDbHelper
              .getP2WPKHAddress(pubkey, segwitPath, networkParameters)
          case _: HDPath =>
            throw new IllegalArgumentException(
              "P2PKH and nested segwit P2PKH not yet implemented")
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
  private def getNewChangeAddress(
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

  /**
    * @inheritdoc
    */
  override def unlock(passphrase: AesPassword): Future[UnlockWalletResult] = ???

  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAll()

  /**
    * @inheritdoc
    */
  // override def createNewAccount: Future[Try[WalletApi]] = ???

}

// todo: create multiple wallets, need to maintain multiple databases
object Wallet extends CreateWalletApi with BitcoinSLogger {

  // The default HD purpose of the bitcoin-s wallet. Can be
  // one of segwit, nested segwit or legacy. Hard coded for
  // now, could be make configurable in the future
  private[wallet] val DEFAULT_HD_PURPOSE: HDPurpose = HDPurposes.SegWit

  private case class WalletImpl(
      mnemonicCode: MnemonicCode,
      walletAppConfig: WalletAppConfig)(implicit val ec: ExecutionContext)
      extends Wallet {

    // todo: until we've figured out a better schem
    override val passphrase: AesPassword = Wallet.badPassphrase
  }

  def apply(mnemonicCode: MnemonicCode, walletAppConfig: WalletAppConfig)(
      implicit ec: ExecutionContext): Wallet =
    WalletImpl(mnemonicCode, walletAppConfig)(ec)

  // todo figure out how to handle password part of wallet
  val badPassphrase = AesPassword("changeMe")

  // todo fix signature
  override def initializeWithEntropy(
      entropy: BitVector,
      appConfig: WalletAppConfig)(
      implicit ec: ExecutionContext): Future[InitializeWalletResult] = {
    import org.bitcoins.core.util.EitherUtil.EitherOps._

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
        val encryptedT = EncryptedMnemonicHelper
          .encrypt(mnemonic, badPassphrase)

        val encryptedE: Either[Throwable, EncryptedMnemonic] =
          encryptedT match {
            case Failure(exception) => Left(exception)
            case Success(value)     => Right(value)
          }

        encryptedE.left
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
        val coin =
          HDCoin(DEFAULT_HD_PURPOSE, HDUtil.getCoinType(chainParams.network))
        val account = HDAccount(coin, 0)
        val xpriv = wallet.xprivForPurpose(DEFAULT_HD_PURPOSE)

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
