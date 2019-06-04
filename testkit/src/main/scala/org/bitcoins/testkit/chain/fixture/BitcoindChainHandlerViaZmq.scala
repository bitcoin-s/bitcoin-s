package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.fixture
import org.bitcoins.zmq.ZMQSubscriber

/** Represents a bitcoind instance paired with a chain handler via zmq */
case class BitcoindChainHandlerViaZmq(
    bitcoindRpc: BitcoindRpcClient,
    chainHandler: ChainHandler,
    zmqSubscriber: ZMQSubscriber)

object BitcoindChainHandlerViaZmq {

  def apply(
      bitcoindRpc: BitcoindRpcClient,
      pair: (ChainHandler, ZMQSubscriber)): BitcoindChainHandlerViaZmq = {
    val (chainHandler, zmqSubscriber) = pair

    fixture.BitcoindChainHandlerViaZmq(bitcoindRpc, chainHandler, zmqSubscriber)
  }
}
