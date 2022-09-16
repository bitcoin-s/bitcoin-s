package org.bitcoins.dlc.node

import grizzled.slf4j.Logging
import org.bitcoins.core.api.callback.{CallbackFactory, ModuleCallbacks}
import org.bitcoins.core.api.{Callback, CallbackHandler}

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

/** Callbacks for responding to events in the DLC node. */
trait DLCNodeCallbacks extends ModuleCallbacks[DLCNodeCallbacks] with Logging {

  def onPeerConnectionInitiated: CallbackHandler[
    InetSocketAddress,
    OnPeerConnectionInitiated]

  def onPeerConnectionEstablished: CallbackHandler[
    InetSocketAddress,
    OnPeerConnectionEstablished]

  def onPeerConnectionFailed: CallbackHandler[
    InetSocketAddress,
    OnPeerConnectionFailed]

  override def +(other: DLCNodeCallbacks): DLCNodeCallbacks

  def executeOnPeerConnectionInitiated(peerAddress: InetSocketAddress)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onPeerConnectionInitiated.execute(
      peerAddress,
      (err: Throwable) =>
        logger.error(
          s"${onPeerConnectionInitiated.name} Callback failed with error: ",
          err))
  }

  def executeOnPeerConnectionEstablished(peerAddress: InetSocketAddress)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onPeerConnectionEstablished.execute(
      peerAddress,
      (err: Throwable) =>
        logger.error(
          s"${onPeerConnectionEstablished.name} Callback failed with error: ",
          err))
  }

  def executeOnPeerConnectionFailed(peerAddress: InetSocketAddress)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onPeerConnectionFailed.execute(
      peerAddress,
      (err: Throwable) =>
        logger.error(
          s"${onPeerConnectionFailed.name} Callback failed with error: ",
          err))
  }
}

trait OnPeerConnectionInitiated extends Callback[InetSocketAddress]

trait OnPeerConnectionEstablished extends Callback[InetSocketAddress]

trait OnPeerConnectionFailed extends Callback[InetSocketAddress]

object DLCNodeCallbacks extends CallbackFactory[DLCNodeCallbacks] {

  // Use Impl pattern here to enforce the correct names on the CallbackHandlers
  private case class DLCNodeCallbacksImpl(
      onPeerConnectionInitiated: CallbackHandler[
        InetSocketAddress,
        OnPeerConnectionInitiated],
      onPeerConnectionEstablished: CallbackHandler[
        InetSocketAddress,
        OnPeerConnectionEstablished],
      onPeerConnectionFailed: CallbackHandler[
        InetSocketAddress,
        OnPeerConnectionFailed])
      extends DLCNodeCallbacks {

    override def +(other: DLCNodeCallbacks): DLCNodeCallbacks =
      copy(
        onPeerConnectionInitiated =
          onPeerConnectionInitiated ++ other.onPeerConnectionInitiated,
        onPeerConnectionEstablished =
          onPeerConnectionEstablished ++ other.onPeerConnectionEstablished,
        onPeerConnectionFailed =
          onPeerConnectionFailed ++ other.onPeerConnectionFailed
      )
  }

  def onPeerConnectionInitiated(
      f: OnPeerConnectionInitiated): DLCNodeCallbacks =
    DLCNodeCallbacks(onPeerConnectionInitiated = Vector(f))

  def onPeerConnectionEstablished(
      f: OnPeerConnectionEstablished): DLCNodeCallbacks =
    DLCNodeCallbacks(onPeerConnectionEstablished = Vector(f))

  def onPeerConnectionFailed(f: OnPeerConnectionFailed): DLCNodeCallbacks =
    DLCNodeCallbacks(onPeerConnectionFailed = Vector(f))

  /** Empty callbacks that does nothing with the received data */
  override val empty: DLCNodeCallbacks =
    DLCNodeCallbacks(Vector.empty, Vector.empty, Vector.empty)

  def apply(
      onPeerConnectionInitiated: Vector[OnPeerConnectionInitiated] =
        Vector.empty,
      onPeerConnectionEstablished: Vector[OnPeerConnectionEstablished] =
        Vector.empty,
      onPeerConnectionFailed: Vector[OnPeerConnectionFailed] =
        Vector.empty): DLCNodeCallbacks = {
    DLCNodeCallbacksImpl(
      onPeerConnectionInitiated =
        CallbackHandler[InetSocketAddress, OnPeerConnectionInitiated](
          "onPeerConnectionInitiated",
          onPeerConnectionInitiated),
      onPeerConnectionEstablished =
        CallbackHandler[InetSocketAddress, OnPeerConnectionEstablished](
          "onPeerConnectionEstablished",
          onPeerConnectionEstablished),
      onPeerConnectionFailed =
        CallbackHandler[InetSocketAddress, OnPeerConnectionFailed](
          "onPeerConnectionFailed",
          onPeerConnectionFailed)
    )
  }
}
