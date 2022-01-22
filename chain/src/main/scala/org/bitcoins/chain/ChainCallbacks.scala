package org.bitcoins.chain

import grizzled.slf4j.Logger
import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}

trait ChainCallbacks {

  def onBlockHeaderConnected: CallbackHandler[
    Vector[(Int, BlockHeader)],
    OnBlockHeaderConnected]

  def +(other: ChainCallbacks): ChainCallbacks

  def executeOnBlockHeaderConnectedCallbacks(
      logger: Logger,
      heightHeaderTuple: Vector[(Int, BlockHeader)])(implicit
      ec: ExecutionContext): Future[Unit] = {

    onBlockHeaderConnected.execute(
      heightHeaderTuple,
      (err: Throwable) =>
        logger.error(
          s"${onBlockHeaderConnected.name} Callback failed with error: ",
          err))
  }

}

/** Callback for handling a received block header */
trait OnBlockHeaderConnected extends Callback[Vector[(Int, BlockHeader)]]

object ChainCallbacks {

  private case class ChainCallbacksImpl(
      onBlockHeaderConnected: CallbackHandler[
        Vector[(Int, BlockHeader)],
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
      CallbackHandler[Vector[(Int, BlockHeader)], OnBlockHeaderConnected](
        "onBlockHeaderConnected",
        onBlockHeaderConnected))
}
