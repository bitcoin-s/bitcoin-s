package org.bitcoins.core.api.node

import org.bitcoins.core.api.chain.ChainQueryApi

case class NodeChainQueryApi(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)
