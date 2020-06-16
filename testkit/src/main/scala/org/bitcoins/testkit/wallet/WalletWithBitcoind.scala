package org.bitcoins.testkit.wallet

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.wallet.Wallet


sealed trait WalletWithBitcoind {
  def wallet: Wallet
  def bitcoind: BitcoindRpcClient
}
case class WalletWithBitcoindRpc(wallet: Wallet, bitcoind: BitcoindRpcClient)
  extends WalletWithBitcoind
case class WalletWithBitcoindV19(
                                  wallet: Wallet,
                                  bitcoind: BitcoindV19RpcClient)
  extends WalletWithBitcoind
