package org.bitcoins.core.api.callback

import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.blockchain.Block

import scala.concurrent.{ExecutionContext, Future}

/** Callback for handling a received block */
trait OnBlockReceived extends Callback[Block]

trait NodeApiCallbacks {
  def onBlockReceived: CallbackHandler[Block, OnBlockReceived]
  def executeOnBlockReceivedCallbacks(
      block: Block
  )(implicit ec: ExecutionContext): Future[Unit] = {
    onBlockReceived.execute(block)
  }
}
