package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.AddressType._
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.blockchain.{
  MainNetChainParams,
  RegTestNetChainParams,
  SigNetChainParams,
  TestNetChainParams
}
import org.bitcoins.wallet.Wallet

import scala.concurrent.Future

/**
  * Provides functionality related enumerating accounts. Account
  * creation does not happen here, as that requires an unlocked wallet.
  */
private[wallet] trait AccountHandling { self: Wallet =>

  /** @inheritdoc */
  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  private def getOrThrowAccount(account: Option[AccountDb]): AccountDb =
    account.getOrElse(
      throw new RuntimeException(
        s"Could not find account with ${DEFAULT_HD_COIN.purpose.constant} " +
          s"purpose field and ${DEFAULT_HD_COIN.coinType.toInt} coin field"))

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
      addressType: AddressType): Future[AccountDb] = {
    val hdCoin = addressType match {
      case Legacy       => HDCoin(HDPurposes.Legacy, DEFAULT_HD_COIN_TYPE)
      case NestedSegWit => HDCoin(HDPurposes.NestedSegWit, DEFAULT_HD_COIN_TYPE)
      case SegWit       => HDCoin(HDPurposes.SegWit, DEFAULT_HD_COIN_TYPE)
    }
    for {
      account <- accountDAO.read((hdCoin, 0))
    } yield getOrThrowAccount(account)
  }

  /** The default HD coin for this wallet, read from config */
  protected[wallet] lazy val DEFAULT_HD_COIN: HDCoin = {
    val coinType = DEFAULT_HD_COIN_TYPE
    HDCoin(walletConfig.defaultAccountKind, coinType)
  }

  /** The default HD coin type for this wallet, derived from
    * the network we're on
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
