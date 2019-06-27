package org.bitcoins.wallet.internal

import org.bitcoins.wallet.LockedWallet
import scala.concurrent.Future
import org.bitcoins.wallet.models.AccountDb
import org.bitcoins.core.hd.HDCoinType
import org.bitcoins.core.hd.HDCoin
import org.bitcoins.core.protocol.blockchain.TestNetChainParams
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.core.protocol.blockchain.MainNetChainParams

/**
  * Provides functionality related enumerating accounts. Account
  * creation does not happen here, as that requires an unlocked wallet.
  */
private[wallet] trait AccountHandling { self: LockedWallet =>

  /** @inheritdoc */
  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

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

  /** The default HD coin for this wallet, read from config */
  protected[wallet] lazy val DEFAULT_HD_COIN: HDCoin = {
    val coinType = chainParams match {
      case MainNetChainParams                         => HDCoinType.Bitcoin
      case RegTestNetChainParams | TestNetChainParams => HDCoinType.Testnet
    }
    HDCoin(walletConfig.defaultAccountKind, coinType)
  }
}
