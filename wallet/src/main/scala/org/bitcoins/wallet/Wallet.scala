package org.bitcoins.wallet
import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto.bip44._
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.{BitcoinSLogger, EitherUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.db.DbConfig
import org.bitcoins.wallet.api._
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
  override def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[Either[AddUtxoError, WalletApi]] = {
    import AddUtxoError._

    logger.trace(
      s"Adding UTXO to wallet. TXID: ${transaction.txId.hex}, vout: ${vout.toInt}")

    // first check: does the provided vout exist in the tx?
    val voutLength = transaction.outputs.length
    val voutIndexOutOfBounds = {
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
          case None =>
            logger.error(
              s"Constructed address $address from provided tx and vout does not exist in our DB!")
            Left(AddressNotFound)
        }
      EitherUtil.flattenFutureE(nestedAddressE)
    }

    // insert the UTXO into the DB
    addressDbEitherF.flatMap { addressDbE =>
      val nestedWalletE: Either[
        AddUtxoError,
        Future[Either[AddUtxoError, WalletApi]]] =
        for {
          output <- outputE
          outPoint <- outPointE
          addressDb <- addressDbE
        } yield {
          val utxo =
            UTXOSpendingInfoDb(
              id = None,
              outPoint = outPoint,
              output = output,
              privKeyPath = addressDb.path,
              redeemScriptOpt = None, // todo fix me when implementing P2SH addresses
              scriptWitnessOpt = addressDb.witnessScriptOpt
            )
          utxoDAO.create(utxo).map { _ =>
            logger.trace(
              s"Successfully inserted UTXO ${transaction.txId.hex}:${vout.toInt} into DB")
            Right(this.asInstanceOf[WalletApi])
          }
        }
      EitherUtil.flattenFutureE(nestedWalletE)
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
      accountIndex: Int = 0): Future[Transaction] = {
    for {
      change <- getNewChangeAddress(accountIndex)
      fee <- feeProvider.getFee(1)
      walletUtxos <- listUtxos()
      txBuilder <- {
        val destinations: Seq[TransactionOutput] = List(
          TransactionOutput(amount, address.scriptPubKey))

        // currencly just grabs one utxos, throws if can't find big enough
        // todo: implement coin selection
        val utxos: List[BitcoinUTXOSpendingInfo] =
          List(walletUtxos.find(_.value >= amount).get.toUTXOSpendingInfo(this))

        BitcoinTxBuilder(destinations,
                         utxos,
                         fee,
                         changeSPK = change.scriptPubKey,
                         networkParameters)
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
  override def unlock(passphrase: String): Future[Try[UnlockedWalletApi]] = ???

  /**
    * @inheritdoc
    */
  // override def getAccounts: Future[Vector[BIP44Account]] = ???

  /**
    * @inheritdoc
    */
  // override def createNewAccount: Future[Try[WalletApi]] = ???

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
