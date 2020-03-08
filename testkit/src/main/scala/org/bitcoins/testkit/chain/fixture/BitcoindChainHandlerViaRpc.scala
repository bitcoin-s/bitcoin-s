package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.rpc.client.common.BitcoindRpcClient

/** Represents a bitcoind instance paired with a chain handler via rpc */
case class BitcoindChainHandlerViaRpc(
    bitcoindRpc: BitcoindRpcClient,
    chainHandler: ChainHandler)
