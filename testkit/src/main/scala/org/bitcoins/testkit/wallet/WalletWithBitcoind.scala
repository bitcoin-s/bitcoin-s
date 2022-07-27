package org.bitcoins.testkit.wallet

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.wallet.Wallet

sealed trait WalletWithBitcoind[T <: BitcoindRpcClient] {
  def wallet: Wallet
  def bitcoind: T
}

/** General pairing of a wallet with a bitcoind rpc. If you don't care about
  * the version of bitcoind, you should use this.
  */
case class WalletWithBitcoindRpc(wallet: Wallet, bitcoind: BitcoindRpcClient)
    extends WalletWithBitcoind[BitcoindRpcClient]

case class WalletWithBitcoindV19(wallet: Wallet, bitcoind: BitcoindV19RpcClient)
    extends WalletWithBitcoind[BitcoindV19RpcClient]
