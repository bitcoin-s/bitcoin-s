package org.bitcoins.core.api

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.FutureUtil

import scala.concurrent.Future

/**
  * Represent a bitcoin-s node API
  */
trait NodeApi {

  /**
    * Request the underlying node to download the given blocks from its peers and feed the blocks to [[org.bitcoins.node.NodeCallbacks]].
    */
  def downloadBlocks(blockHashes: Vector[DoubleSha256Digest]): Future[Unit]

}

object NodeApi {

  object NoOp extends NodeApi {

    override def downloadBlocks(
        blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = FutureUtil.unit

  }

}
