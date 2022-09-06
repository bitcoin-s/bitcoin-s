package org.bitcoins.dlc.node

import grizzled.slf4j.Logging
import org.bitcoins.core.api.callback.{CallbackFactory, ModuleCallbacks}
import org.bitcoins.core.api.{Callback, CallbackHandler}

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

/** Callbacks for responding to events in the DLC node. */
trait DLCNodeCallbacks extends ModuleCallbacks[DLCNodeCallbacks] with Logging {

  def onPeerConnectionEstablished: CallbackHandler[
    InetSocketAddress,
    OnPeerConnectionEstablished]

  def onPeerConnectionFailed: CallbackHandler[
    (InetSocketAddress, String),
    OnPeerConnectionFailed]

  override def +(other: DLCNodeCallbacks): DLCNodeCallbacks

  def executeOnPeerConnectionEstablished(peerAddress: InetSocketAddress)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onPeerConnectionEstablished.execute(
      peerAddress,
      (err: Throwable) =>
        logger.error(
          s"${onPeerConnectionEstablished.name} Callback failed with error: ",
          err))
  }

  def executeOnPeerConnectionFailed(
      peerAddress: InetSocketAddress,
      ex: Throwable)(implicit ec: ExecutionContext): Future[Unit] = {
    onPeerConnectionFailed.execute(
      (peerAddress, ex.getMessage),
      (err: Throwable) =>
        logger.error(
          s"${onPeerConnectionFailed.name} Callback failed with error: ",
          err))
  }
}

trait OnPeerConnectionEstablished extends Callback[InetSocketAddress]

trait OnPeerConnectionFailed extends Callback[(InetSocketAddress, String)]

object DLCNodeCallbacks extends CallbackFactory[DLCNodeCallbacks] {

  // Use Impl pattern here to enforce the correct names on the CallbackHandlers
  private case class DLCNodeCallbacksImpl(
      onPeerConnectionEstablished: CallbackHandler[
        InetSocketAddress,
        OnPeerConnectionEstablished],
      onPeerConnectionFailed: CallbackHandler[
        (InetSocketAddress, String),
        OnPeerConnectionFailed])
      extends DLCNodeCallbacks {

    override def +(other: DLCNodeCallbacks): DLCNodeCallbacks =
      copy(
        onPeerConnectionEstablished =
          onPeerConnectionEstablished ++ other.onPeerConnectionEstablished,
        onPeerConnectionFailed =
          onPeerConnectionFailed ++ other.onPeerConnectionFailed
      )
  }

  /** Constructs a set of callbacks that only acts on TX received */
  def onPeerConnectionEstablished(
      f: OnPeerConnectionEstablished): DLCNodeCallbacks =
    DLCNodeCallbacks(onPeerConnectionEstablished = Vector(f))

  def onPeerConnectionFailed(f: OnPeerConnectionFailed): DLCNodeCallbacks =
    DLCNodeCallbacks(onPeerConnectionFailed = Vector(f))

  /** Empty callbacks that does nothing with the received data */
  override val empty: DLCNodeCallbacks =
    DLCNodeCallbacks(Vector.empty, Vector.empty)

  def apply(
      onPeerConnectionEstablished: Vector[OnPeerConnectionEstablished] =
        Vector.empty,
      onPeerConnectionFailed: Vector[OnPeerConnectionFailed] =
        Vector.empty): DLCNodeCallbacks = {
    DLCNodeCallbacksImpl(
      onPeerConnectionEstablished =
        CallbackHandler[InetSocketAddress, OnPeerConnectionEstablished](
          "onPeerConnectionEstablished",
          onPeerConnectionEstablished),
      onPeerConnectionFailed =
        CallbackHandler[(InetSocketAddress, String), OnPeerConnectionFailed](
          "onPeerConnectionFailed",
          onPeerConnectionFailed)
    )
  }
}
