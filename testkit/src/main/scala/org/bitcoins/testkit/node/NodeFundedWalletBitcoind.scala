package org.bitcoins.testkit.node

import org.bitcoins.node.{NeutrinoNode, Node, SpvNode}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.wallet.Wallet

/**
 * Creates
 * 1. a funded bitcoind wallet
 * 2. a funded bitcoin-s wallet
 * 3. a chain handler with the appropriate tables created
 * 4. a spv node that is connected to the bitcoin instance -- but not started!  */
trait NodeFundedWalletBitcoind {
  def node: Node
  def wallet: Wallet
  def bitcoindRpc: BitcoindRpcClient
  def bip39PasswordOpt: Option[String]
}
case class SpvNodeFundedWalletBitcoind(
                                        node: SpvNode,
                                        wallet: Wallet,
                                        bitcoindRpc: BitcoindRpcClient,
                                        bip39PasswordOpt: Option[String])
  extends NodeFundedWalletBitcoind
case class NeutrinoNodeFundedWalletBitcoind(
                                             node: NeutrinoNode,
                                             wallet: Wallet,
                                             bitcoindRpc: BitcoindRpcClient,
                                             bip39PasswordOpt: Option[String])
  extends NodeFundedWalletBitcoind

