package org.bitcoins.core.node

import org.bitcoins.core.crypto.DoubleSha256Digest

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
