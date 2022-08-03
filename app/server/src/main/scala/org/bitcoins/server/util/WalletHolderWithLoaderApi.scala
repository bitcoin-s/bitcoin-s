package org.bitcoins.server.util

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.{
  DLCWalletBitcoindBackendLoader,
  DLCWalletLoaderApi,
  DLCWalletNeutrinoBackendLoader
}
import org.bitcoins.wallet.WalletHolder

sealed trait WalletHolderWithLoaderApi {
  def walletHolder: WalletHolder
  def loaderApi: DLCWalletLoaderApi
}

case class WalletHolderWithNeutrinoLoaderApi(
    walletHolder: WalletHolder,
    loaderApi: DLCWalletNeutrinoBackendLoader)
    extends WalletHolderWithLoaderApi

case class WalletHolderWithBitcoindLoaderApi(
    walletHolder: WalletHolder,
    loaderApi: DLCWalletBitcoindBackendLoader)
    extends WalletHolderWithLoaderApi {
  val bitcoind: BitcoindRpcClient = loaderApi.bitcoind
}
