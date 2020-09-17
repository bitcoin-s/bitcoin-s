package org.bitcoins.chain

import org.bitcoins.core.api.{Callback2, CallbackHandler}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}

trait ChainCallbacks {

  def onBlockHeaderConnected: CallbackHandler[
    (Int, BlockHeader),
    OnBlockHeaderConnected]

  def +(other: ChainCallbacks): ChainCallbacks

  def executeOnBlockHeaderConnectedCallbacks(
      logger: Logger,
      height: Int,
      header: BlockHeader)(implicit ec: ExecutionContext): Future[Unit] = {
    onBlockHeaderConnected.execute(logger, (height, header))
  }

}

/** Callback for handling a received block header */
trait OnBlockHeaderConnected extends Callback2[Int, BlockHeader]

object ChainCallbacks {

  private case class ChainCallbacksImpl(
      onBlockHeaderConnected: CallbackHandler[
        (Int, BlockHeader),
        OnBlockHeaderConnected])
      extends ChainCallbacks {

    override def +(other: ChainCallbacks): ChainCallbacks =
      copy(onBlockHeaderConnected =
        onBlockHeaderConnected ++ other.onBlockHeaderConnected)
  }

  /** Constructs a set of callbacks that only acts on block headers connected */
  def onBlockHeaderConnected(f: OnBlockHeaderConnected): ChainCallbacks =
    ChainCallbacks(onBlockHeaderConnected = Vector(f))

  lazy val empty: ChainCallbacks =
    ChainCallbacks(onBlockHeaderConnected = Vector.empty)

  def apply(
      onBlockHeaderConnected: Vector[OnBlockHeaderConnected] =
        Vector.empty): ChainCallbacks =
    ChainCallbacksImpl(onBlockHeaderConnected =
      CallbackHandler[(Int, BlockHeader), OnBlockHeaderConnected](
        "onBlockHeaderConnected",
        onBlockHeaderConnected))
}
