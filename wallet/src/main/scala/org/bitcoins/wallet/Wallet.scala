package org.bitcoins.wallet

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress, BlockStamp}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.dlc.DLCMessage.{DLCAccept, DLCOffer, DLCSign, OracleInfo}
import org.bitcoins.dlc.{BinaryOutcomeDLCClient, DLCTimeouts}
import org.bitcoins.keymanager.KeyManagerParams
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.keymanager.util.HDUtil
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

  /** We need a output so we can get proper tx size estimation */
  private def getDummyDLCFundingOutputs(
      amount: CurrencyUnit): Vector[TransactionOutput] = {
    val key = ECPublicKey.freshPublicKey
    val spk = P2WSHWitnessSPKV0(MultiSignatureScriptPubKey(2, Seq(key, key)))

    // funded output and the other party's change
    Vector(TransactionOutput(amount, spk), TransactionOutput(1.satoshi, spk))
  }

  override def createDLCOffer(
      amount: Bitcoins,
      oracleInfo: OracleInfo,
      contractInfo: Vector[Sha256DigestBE],
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLocktime: UInt32): Future[DLCOffer] = {
    val feeRate = feeRateOpt.getOrElse(SatoshisPerVirtualByte.one)
    for {
      account <- getDefaultAccount()
      txBuilder <- fundRawTransactionInternal(
        destinations = getDummyDLCFundingOutputs(amount),
        feeRate = feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager)
      )
    } yield {
      val contractInfoMap =
        (Map.newBuilder ++= contractInfo.map(sha => (sha, amount.satoshis)))
          .result()

      DLCOffer(
        contractInfoMap,
        oracleInfo,
        keyManager.getRootXPub,
        amount.satoshis,
        txBuilder.utxoMap
          .filter(_._2.amount != Satoshis.one)
          .map(utxo => (utxo._1, utxo._2.output))
          .toVector,
        Bech32Address(txBuilder.changeSPK.asInstanceOf[WitnessScriptPubKey],
                      keyManager.kmParams.network),
        feeRate,
        DLCTimeouts(5,
                    BlockStamp(locktime.toInt),
                    BlockStamp(refundLocktime.toInt))
      )
    }
  }

  override def acceptDLCOffer(
      dlcOffer: DLCOffer,
      amount: Bitcoins): Future[DLCAccept] = {
    val clientF = for {
      account <- getDefaultAccount()
      txBuilder <- fundRawTransactionInternal(
        destinations = getDummyDLCFundingOutputs(amount),
        feeRate = dlcOffer.feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager)
      )
    } yield {
      val fundingUtxos =
        txBuilder.utxoMap.values
          .flatMap(_.toSingles)
          .toVector
      val changeSPK = txBuilder.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      val client = BinaryOutcomeDLCClient.fromOffer(
        dlcOffer,
        keyManager.rootExtPrivKey, // todo change to a ExtSign.deriveAndSignFuture // fixme this will need to be changed according to KeyDerivation.md
        fundingUtxos,
        amount,
        dlcOffer.totalCollateral + amount,
        dlcOffer.totalCollateral + amount, //todo remove these for binary cases after refactor
        changeSPK
      )

      (client, txBuilder, changeSPK)
    }

    val f = clientF.flatMap {
      case (client, txBuilder, changeSPK) =>
        client.createCETSigs.map(
          sigs =>
            DLCAccept(
              amount.satoshis,
              keyManager.getRootXPub,
              txBuilder.utxoMap.map(utxo => (utxo._1, utxo._2.output)).toVector,
              Bech32Address(changeSPK, keyManager.kmParams.network),
              sigs
            )
        )
    }
    f.failed.foreach { err =>
      err.printStackTrace()
      logger.error(err.getLocalizedMessage)
    }
    f
  }

  def signDLC(offer: DLCOffer, accept: DLCAccept): Future[DLCSign] = {
    val clientF = for {
      account <- getDefaultAccount()
      txBuilder <- fundRawTransactionInternal(
        destinations = getDummyDLCFundingOutputs(offer.totalCollateral),
        feeRate = offer.feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager)
      )
    } yield {
      val fundingUtxos =
        txBuilder.utxoMap.values
          .flatMap(_.toSingles)
          .toVector
      BinaryOutcomeDLCClient.fromOfferAndAccept(
        offer,
        accept,
        keyManager.rootExtPrivKey,
        fundingUtxos,
        offer.totalCollateral + accept.totalCollateral,
        offer.totalCollateral + accept.totalCollateral)
    }

    clientF.flatMap { client =>
      client.createCETSigs.zip(client.createFundingTransactionSigs()).map {
        case (cetSigs, fundingSigs) =>
          DLCSign(cetSigs, fundingSigs)
      }
    }
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
        keyManagerOpt = Some(keyManager))
      signed <- txBuilder.sign
      ourOuts <- findOurOuts(signed)
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
