package org.bitcoins.chain

import org.bitcoins.core.api.callback.{CallbackFactory, ModuleCallbacks}
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import slick.util.Logging

import scala.concurrent.{ExecutionContext, Future}

trait ChainCallbacks extends ModuleCallbacks[ChainCallbacks] with Logging {

  def onBlockHeaderConnected: CallbackHandler[
    Vector[(Int, BlockHeader)],
    OnBlockHeaderConnected]

  def onCompactFilterHeaderConnected: CallbackHandler[
    Vector[CompactFilterHeaderDb],
    OnCompactFilterHeaderConnected]

  def onCompactFilterConnected: CallbackHandler[
    Vector[CompactFilterDb],
    OnCompactFilterConnected]

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

  def executeOnCompactFilterHeaderConnectedCallbacks(
      filterHeaders: Vector[CompactFilterHeaderDb])(implicit
      ec: ExecutionContext): Future[Unit] = {
    onCompactFilterHeaderConnected.execute(
      filterHeaders,
      (err: Throwable) =>
        logger.error(
          s"${onCompactFilterHeaderConnected.name} Callback failed with err",
          err))
  }

  def executeOnCompactFilterConnectedCallbacks(
      filters: Vector[CompactFilterDb])(implicit
      ec: ExecutionContext): Future[Unit] = {
    onCompactFilterConnected.execute(
      filters,
      (err: Throwable) =>
        logger.error(
          s"${onCompactFilterConnected.name} Callback failed with err",
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

trait OnCompactFilterHeaderConnected
    extends Callback[Vector[CompactFilterHeaderDb]]

trait OnCompactFilterConnected extends Callback[Vector[CompactFilterDb]]

trait OnSyncFlagChanged extends Callback[Boolean]

object ChainCallbacks extends CallbackFactory[ChainCallbacks] {

  private case class ChainCallbacksImpl(
      onBlockHeaderConnected: CallbackHandler[
        Vector[(Int, BlockHeader)],
        OnBlockHeaderConnected],
      onCompactFilterHeaderConnected: CallbackHandler[
        Vector[CompactFilterHeaderDb],
        OnCompactFilterHeaderConnected],
      onCompactFilterConnected: CallbackHandler[
        Vector[CompactFilterDb],
        OnCompactFilterConnected],
      onSyncFlagChanged: CallbackHandler[Boolean, OnSyncFlagChanged])
      extends ChainCallbacks {

    override def +(other: ChainCallbacks): ChainCallbacks =
      copy(
        onBlockHeaderConnected =
          onBlockHeaderConnected ++ other.onBlockHeaderConnected,
        onCompactFilterHeaderConnected =
          onCompactFilterHeaderConnected ++ other.onCompactFilterHeaderConnected,
        onCompactFilterConnected =
          onCompactFilterConnected ++ other.onCompactFilterConnected,
        onSyncFlagChanged = onSyncFlagChanged ++ other.onSyncFlagChanged
      )
  }

  /** Constructs a set of callbacks that only acts on block headers connected */
  def onBlockHeaderConnected(f: OnBlockHeaderConnected): ChainCallbacks =
    ChainCallbacks(onBlockHeaderConnected = Vector(f))

  def onCompactFilterHeaderConnected(
      f: OnCompactFilterHeaderConnected): ChainCallbacks = {
    ChainCallbacks(onCompactFilterHeaderConnected = Vector(f))
  }

  def onCompactFilterConnected(f: OnCompactFilterConnected): ChainCallbacks = {
    ChainCallbacks(onCompactFilterConnected = Vector(f))
  }

  def onOnSyncFlagChanged(f: OnSyncFlagChanged): ChainCallbacks =
    ChainCallbacks(onSyncFlagChanged = Vector(f))

  override val empty: ChainCallbacks =
    ChainCallbacks(onBlockHeaderConnected = Vector.empty)

  def apply(
      onBlockHeaderConnected: Vector[OnBlockHeaderConnected] = Vector.empty,
      onCompactFilterHeaderConnected: Vector[OnCompactFilterHeaderConnected] =
        Vector.empty,
      onCompactFilterConnected: Vector[OnCompactFilterConnected] = Vector.empty,
      onSyncFlagChanged: Vector[OnSyncFlagChanged] =
        Vector.empty): ChainCallbacks =
    ChainCallbacksImpl(
      onBlockHeaderConnected =
        CallbackHandler[Vector[(Int, BlockHeader)], OnBlockHeaderConnected](
          "onBlockHeaderConnected",
          onBlockHeaderConnected),
      onCompactFilterHeaderConnected =
        CallbackHandler[Vector[CompactFilterHeaderDb],
                        OnCompactFilterHeaderConnected](
          "onCompactFilterHeaderConnected",
          onCompactFilterHeaderConnected),
      onCompactFilterConnected =
        CallbackHandler[Vector[CompactFilterDb], OnCompactFilterConnected](
          "onCompactFilterConnected",
          onCompactFilterConnected),
      onSyncFlagChanged =
        CallbackHandler[Boolean, OnSyncFlagChanged]("onSyncFlagChanged",
                                                    onSyncFlagChanged)
    )
}
