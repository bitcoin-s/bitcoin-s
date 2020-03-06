---
id: node-api
title: node-api
---

### NodeApi

The NodeApi is how the wallet project retrieves relevant node data like blocks.
This allows the wallet for example to retrieve blocks for finding its relevant transactions.

Since this is an API it can be hooked up to the `node` module of bitcoin-s but it can also be linked to
any other implementation of your choosing. This allows you to use the bitcoin-s wallet in any schema that you
want.

The functions that the NodeApi supports are:

```scala mdoc
import org.bitcoins.core.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

  /** Request the underlying node to download the given blocks from its peers and feed the blocks to [[org.bitcoins.node.NodeCallbacks]] */
    def downloadBlocks(blockHashes: Vector[DoubleSha256Digest]): Future[Unit]
```