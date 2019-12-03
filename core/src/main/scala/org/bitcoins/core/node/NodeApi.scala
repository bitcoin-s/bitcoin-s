package org.bitcoins.core.node

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.FutureUtil

import scala.concurrent.Future

/**
  * Represent a bitcoin-s node API
  */
trait NodeApi {

  /**
    * Fetches the given blocks from the peers.
    */
  def fetchBlocks(blockHashes: Vector[DoubleSha256Digest]): Future[Unit]

}

object NodeApi {

  object NoOp extends NodeApi {

    override def fetchBlocks(
        blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = FutureUtil.unit

  }

}
