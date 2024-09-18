package org.bitcoins.wallet.internal

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.wallet.AccountHandlingApi
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.AddressType.*
import org.bitcoins.core.hd.*
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  SigNetChainParams,
  TestNetChainParams
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  ScriptPubKeyDAO,
  SpendingInfoDAO,
  WalletDAOs
}
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.{ExecutionContext, Future}

/** Provides functionality related enumerating accounts. Account creation does
  * not happen here, as that requires an unlocked wallet.
  */
case class AccountHandling(
    addressHandling: AddressHandling,
    walletDAOs: WalletDAOs)(implicit
    walletConfig: WalletAppConfig,
    ec: ExecutionContext)
    extends AccountHandlingApi
    with BitcoinSLogger {
  private val accountDAO: AccountDAO = walletDAOs.accountDAO
  private val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val scriptPubKeyDAO: ScriptPubKeyDAO = walletDAOs.scriptPubKeyDAO
  private val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase
  private val chainParams: ChainParams = walletConfig.chain

  /** @inheritdoc */
  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  private def getOrThrowAccount(account: Option[AccountDb]): AccountDb =
    account.getOrElse(
      throw new RuntimeException(
        s"Could not find account with ${DEFAULT_HD_COIN.purpose.constant} " +
          s"purpose field and ${DEFAULT_HD_COIN.coinType.toInt} coin field"
      )
    )

  /** @inheritdoc */
  override def getDefaultAccount(): Future[AccountDb] = {
    for {
      account <- accountDAO.read((DEFAULT_HD_COIN, 0))
    } yield {

      val acct = getOrThrowAccount(account)
      require(
        acct.hdAccount == walletConfig.defaultAccount,
        s"Divergence between configured default account and " +
          s"database default account walletConfig=${walletConfig.defaultAccount} database=${acct.hdAccount}"
      )
      acct
    }
  }

  /** @inheritdoc */
  override def getDefaultAccountForType(
      addressType: AddressType
  ): Future[AccountDb] = {
    val hdCoin = addressType match {
      case Legacy       => HDCoin(HDPurpose.Legacy, DEFAULT_HD_COIN_TYPE)
      case NestedSegWit => HDCoin(HDPurpose.NestedSegWit, DEFAULT_HD_COIN_TYPE)
      case SegWit       => HDCoin(HDPurpose.SegWit, DEFAULT_HD_COIN_TYPE)
      case P2TR =>
        throw new UnsupportedOperationException(
          s"Taproot not supported in wallet")
    }
    for {
      account <- accountDAO.read((hdCoin, 0))
    } yield getOrThrowAccount(account)
  }

  override def clearUtxos(account: HDAccount): Future[Unit] = {
    val aggregatedActions
        : DBIOAction[Unit, NoStream, Effect.Read with Effect.Write] = {
      for {
        accountUtxos <- spendingInfoDAO.findAllForAccountAction(account)
        _ <- spendingInfoDAO.deleteSpendingInfoDbAllAction(accountUtxos)
      } yield ()
    }

    safeDatabase.run(aggregatedActions)
  }

  override def generateScriptPubKeys(
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): Future[Vector[ScriptPubKey]] = {
    val action = generateScriptPubKeysAction(
      account = account,
      addressBatchSize = addressBatchSize,
      forceGenerateSpks = forceGenerateSpks
    )
    safeDatabase.run(action)
  }

  /** If forceGeneratSpks is true or addressCount == 0 we generate a new pool of
    * scriptpubkeys
    */
  private def generateScriptPubKeysAction(
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): DBIOAction[Vector[
                  ScriptPubKey
                ],
                NoStream,
                Effect.Read with Effect.Write with Effect.Transactional] = {
    val addressCountA = addressDAO.countAction
    for {
      addressCount <- addressCountA
      addresses <- {
        if (forceGenerateSpks || addressCount == 0) {
          logger.info(
            s"Generating $addressBatchSize fresh addresses for the rescan"
          )
          generateAddressesForRescanAction(account, addressBatchSize)
        } else {
          // we don't want to continously generate addresses
          // if our wallet already has them, so just use what is in the
          // database already
          addressDAO.findAllAddressesAction().map(_.map(_.address))
        }
      }
      spksDb <- scriptPubKeyDAO.findAllAction()
    } yield {
      val addrSpks =
        addresses.map(_.scriptPubKey)
      val otherSpks = spksDb.map(_.scriptPubKey)

      (addrSpks ++ otherSpks).distinct
    }
  }

  private def generateAddressesForRescanAction(
      account: HDAccount,
      addressBatchSize: Int
  ): DBIOAction[Vector[
                  BitcoinAddress
                ],
                NoStream,
                Effect.Read with Effect.Write with Effect.Transactional] = {
    val receiveAddressesA: DBIOAction[
      Vector[
        BitcoinAddress
      ],
      NoStream,
      Effect.Read with Effect.Write with Effect.Transactional] = {
      DBIOAction.sequence {
        1.to(addressBatchSize)
          .map(_ => addressHandling.getNewAddressAction(account))
      }
    }.map(_.toVector)

    val changeAddressesA: DBIOAction[
      Vector[
        BitcoinAddress
      ],
      NoStream,
      Effect.Read with Effect.Write with Effect.Transactional] = {
      DBIOAction.sequence {
        1.to(addressBatchSize)
          .map(_ => addressHandling.getNewChangeAddressAction(account))
      }
    }.map(_.toVector)

    for {
      receiveAddresses <- receiveAddressesA
      changeAddresses <- changeAddressesA
    } yield receiveAddresses ++ changeAddresses
  }

  /** The default HD coin for this wallet, read from config */
  protected[wallet] lazy val DEFAULT_HD_COIN: HDCoin = {
    val coinType = DEFAULT_HD_COIN_TYPE
    HDCoin(walletConfig.defaultAccountKind, coinType)
  }

  /** The default HD coin type for this wallet, derived from the network we're
    * on
    */
  protected[wallet] lazy val DEFAULT_HD_COIN_TYPE: HDCoinType = {
    chainParams match {
      case MainNetChainParams => HDCoinType.Bitcoin
      case RegTestNetChainParams | TestNetChainParams | SigNetChainParams(_) =>
        HDCoinType.Testnet

    }

  }

  /** The default HD purpose for this wallet, read from config */
  protected[wallet] lazy val DEFAULT_HD_PURPOSE: HDPurpose =
    walletConfig.defaultAccountKind
}
