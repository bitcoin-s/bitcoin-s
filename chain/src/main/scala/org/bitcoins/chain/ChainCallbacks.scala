package org.bitcoins.chain

import org.bitcoins.core.api.callback.{CallbackFactory, ModuleCallbacks}
import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

trait ChainCallbacks extends ModuleCallbacks[ChainCallbacks] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def onBlockHeaderConnected: CallbackHandler[
    Vector[(Int, BlockHeader)],
    OnBlockHeaderConnected]

  def onSyncFlagChanged: CallbackHandler[Boolean, OnSyncFlagChanged]

  override def +(other: ChainCallbacks): ChainCallbacks

  def executeOnBlockHeaderConnectedCallbacks(
      heightHeaderTuple: Vector[(Int, BlockHeader)])(implicit
      ec: ExecutionContext): Future[Unit] = {

    onBlockHeaderConnected.execute(
      heightHeaderTuple,
      (err: Throwable) =>
        logger.error(
          s"${onBlockHeaderConnected.name} Callback failed with error: ",
          err))
  }

  def executeOnSyncFlagChanged(syncing: Boolean)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onSyncFlagChanged.execute(
      syncing,
      (err: Throwable) =>
        logger.error(s"${onSyncFlagChanged.name} Callback failed with error: ",
                     err))
  }

}

/** Callback for handling a received block header */
trait OnBlockHeaderConnected extends Callback[Vector[(Int, BlockHeader)]]

trait OnSyncFlagChanged extends Callback[Boolean]

object ChainCallbacks extends CallbackFactory[ChainCallbacks] {

  private case class ChainCallbacksImpl(
      onBlockHeaderConnected: CallbackHandler[
        Vector[(Int, BlockHeader)],
        OnBlockHeaderConnected],
      onSyncFlagChanged: CallbackHandler[Boolean, OnSyncFlagChanged])
      extends ChainCallbacks {

    override def +(other: ChainCallbacks): ChainCallbacks =
      copy(onBlockHeaderConnected =
             onBlockHeaderConnected ++ other.onBlockHeaderConnected,
           onSyncFlagChanged = onSyncFlagChanged ++ other.onSyncFlagChanged)
  }

  /** Constructs a set of callbacks that only acts on block headers connected */
  def onBlockHeaderConnected(f: OnBlockHeaderConnected): ChainCallbacks =
    ChainCallbacks(onBlockHeaderConnected = Vector(f))

  def onOnSyncFlagChanged(f: OnSyncFlagChanged): ChainCallbacks =
    ChainCallbacks(onSyncFlagChanged = Vector(f))

  override val empty: ChainCallbacks =
    ChainCallbacks(onBlockHeaderConnected = Vector.empty)

  def apply(
      onBlockHeaderConnected: Vector[OnBlockHeaderConnected] = Vector.empty,
      onSyncFlagChanged: Vector[OnSyncFlagChanged] =
        Vector.empty): ChainCallbacks =
    ChainCallbacksImpl(
      onBlockHeaderConnected =
        CallbackHandler[Vector[(Int, BlockHeader)], OnBlockHeaderConnected](
          "onBlockHeaderConnected",
          onBlockHeaderConnected),
      onSyncFlagChanged =
        CallbackHandler[Boolean, OnSyncFlagChanged]("onSyncFlagChanged",
                                                    onSyncFlagChanged)
    )
}
