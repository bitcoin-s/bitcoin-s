package org.bitcoins.testkit.wallet

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.wallet.config.WalletAppConfig

sealed trait WalletAppConfigWithBitcoind {
  def bitcoind: BitcoindRpcClient
  def walletAppConfig: WalletAppConfig
}

case class WalletAppConfigWithBitcoindRpc(
    walletAppConfig: WalletAppConfig,
    bitcoind: BitcoindRpcClient)
