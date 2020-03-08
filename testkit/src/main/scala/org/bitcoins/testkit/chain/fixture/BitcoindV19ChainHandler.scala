package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient

/** Useful for neutrino RPCs from bitcoind */
case class BitcoindV19ChainHandler(
    bitcoind: BitcoindV19RpcClient,
    chainHandler: ChainHandler)
