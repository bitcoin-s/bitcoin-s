package org.bitcoins.testkit.node.fixture

import org.bitcoins.node.SpvNode
import org.bitcoins.rpc.client.common.BitcoindRpcClient

/** Gives us a fixture that has a spv node connected with the bitcoind instance */
case class SpvNodeConnectedWithBitcoind(
    spvNode: SpvNode,
    bitcoind: BitcoindRpcClient)
