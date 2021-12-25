package org.bitcoins.testkit.server

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSServerMain

case class ServerWithBitcoind(
    bitcoind: BitcoindRpcClient,
    server: BitcoinSServerMain)
