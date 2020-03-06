---
id: chain-query-api
title: chain-query-api
---


### ChainQueryApi

The ChainQueryApi is how the wallet project stays aware of the current best chain.
This allows the wallet for example to calculate the number of confirmations for a transaction,
get the current chain tip, or even retrieve block filters for a given set of blocks.

Since this is an API it can be hooked up to the `chain` module of bitcoin-s but it can also be linked to
any other implementation of your choosing. This allows you to use the bitcoin-s wallet in any schema that you
want.

The functions that the ChainQueryApi supports are:

```scala mdoc
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.BlockStamp

import scala.concurrent.Future

  /** Gets the height of the given block */
  def getBlockHeight(blockHash: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(): Future[DoubleSha256DigestBE]

  /** Gets number of confirmations for the given block hash*/
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the number of compact filters in the database */
  def getFilterCount: Future[Int]

  /** Returns the block height of the given block stamp */
  def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int]

  def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]]
```