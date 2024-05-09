package org.bitcoins.testkit.wallet

import org.bitcoins.core.api.wallet.NeutrinoHDWalletApi
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.wallet.config.WalletAppConfig

sealed trait WalletWithBitcoind[T <: BitcoindRpcClient] {
  def wallet: NeutrinoHDWalletApi
  def bitcoind: T

  def walletConfig: WalletAppConfig
}

/** General pairing of a wallet with a bitcoind rpc. If you don't care about the
  * version of bitcoind, you should use this.
  */
case class WalletWithBitcoindRpc(
    wallet: NeutrinoHDWalletApi,
    bitcoind: BitcoindRpcClient,
    walletConfig: WalletAppConfig)
    extends WalletWithBitcoind[BitcoindRpcClient]
