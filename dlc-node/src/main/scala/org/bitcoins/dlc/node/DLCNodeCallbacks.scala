package org.bitcoins.dlc.node

import grizzled.slf4j.Logging
import org.bitcoins.core.api.callback.{CallbackFactory, ModuleCallbacks}
import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.crypto.Sha256Digest

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

  def onOfferSendSucceed: CallbackHandler[Sha256Digest, OnOfferSendSucceed]

  def onOfferSendFailed: CallbackHandler[
    (Sha256Digest, String),
    OnOfferSendFailed]

  def onAcceptSucceed: CallbackHandler[Sha256Digest, OnAcceptSucceed]

  def onAcceptFailed: CallbackHandler[(Sha256Digest, String), OnAcceptFailed]

  def onSignSucceed: CallbackHandler[Sha256Digest, OnSignSucceed]

  def onSignFailed: CallbackHandler[(Sha256Digest, String), OnSignFailed]

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

  def executeOnOfferSendSucceed(tempContractId: Sha256Digest)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onOfferSendSucceed.execute(
      tempContractId,
      (err: Throwable) =>
        logger.error(s"${onOfferSendSucceed.name} Callback failed with error: ",
                     err))
  }

  def executeOnOfferSendFailed(
      tempContractId: Sha256Digest,
      errorMessage: String)(implicit ec: ExecutionContext): Future[Unit] = {
    onOfferSendFailed.execute(
      (tempContractId, errorMessage),
      (err: Throwable) =>
        logger.error(s"${onOfferSendFailed.name} Callback failed with error: ",
                     err))
  }

  def executeOnAcceptSucceed(tempContractId: Sha256Digest)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onAcceptSucceed.execute(
      tempContractId,
      (err: Throwable) =>
        logger.error(s"${onAcceptSucceed.name} Callback failed with error: ",
                     err))
  }

  def executeOnAcceptFailed(tempContractId: Sha256Digest, errorMessage: String)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onAcceptFailed.execute(
      (tempContractId, errorMessage),
      (err: Throwable) =>
        logger.error(s"${onAcceptFailed.name} Callback failed with error: ",
                     err))
  }

  def executeOnSignSucceed(tempContractId: Sha256Digest)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onSignSucceed.execute(
      tempContractId,
      (err: Throwable) =>
        logger.error(s"${onSignSucceed.name} Callback failed with error: ",
                     err))
  }

  def executeOnSignFailed(tempContractId: Sha256Digest, errorMessage: String)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onSignFailed.execute(
      (tempContractId, errorMessage),
      (err: Throwable) =>
        logger.error(s"${onSignFailed.name} Callback failed with error: ", err))
  }
}

trait OnPeerConnectionInitiated extends Callback[InetSocketAddress]

trait OnPeerConnectionEstablished extends Callback[InetSocketAddress]

trait OnPeerConnectionFailed extends Callback[InetSocketAddress]

trait OnOfferSendSucceed extends Callback[Sha256Digest]

trait OnOfferSendFailed extends Callback[(Sha256Digest, String)]

trait OnAcceptSucceed extends Callback[Sha256Digest]

trait OnAcceptFailed extends Callback[(Sha256Digest, String)]

trait OnSignSucceed extends Callback[Sha256Digest]

trait OnSignFailed extends Callback[(Sha256Digest, String)]

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
        OnPeerConnectionFailed],
      onOfferSendSucceed: CallbackHandler[Sha256Digest, OnOfferSendSucceed],
      onOfferSendFailed: CallbackHandler[
        (Sha256Digest, String),
        OnOfferSendFailed],
      onAcceptSucceed: CallbackHandler[Sha256Digest, OnAcceptSucceed],
      onAcceptFailed: CallbackHandler[(Sha256Digest, String), OnAcceptFailed],
      onSignSucceed: CallbackHandler[Sha256Digest, OnSignSucceed],
      onSignFailed: CallbackHandler[(Sha256Digest, String), OnSignFailed])
      extends DLCNodeCallbacks {

    override def +(other: DLCNodeCallbacks): DLCNodeCallbacks =
      copy(
        onPeerConnectionInitiated =
          onPeerConnectionInitiated ++ other.onPeerConnectionInitiated,
        onPeerConnectionEstablished =
          onPeerConnectionEstablished ++ other.onPeerConnectionEstablished,
        onPeerConnectionFailed =
          onPeerConnectionFailed ++ other.onPeerConnectionFailed,
        onOfferSendSucceed = onOfferSendSucceed ++ other.onOfferSendSucceed,
        onOfferSendFailed = onOfferSendFailed ++ other.onOfferSendFailed,
        onAcceptSucceed = onAcceptSucceed ++ other.onAcceptSucceed,
        onAcceptFailed = onAcceptFailed ++ other.onAcceptFailed,
        onSignSucceed = onSignSucceed ++ other.onSignSucceed,
        onSignFailed = onSignFailed ++ other.onSignFailed
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
      onPeerConnectionFailed: Vector[OnPeerConnectionFailed] = Vector.empty,
      onOfferSendSucceed: Vector[OnOfferSendSucceed] = Vector.empty,
      onOfferSendFailed: Vector[OnOfferSendFailed] = Vector.empty,
      onAcceptSucceed: Vector[OnAcceptSucceed] = Vector.empty,
      onAcceptFailed: Vector[OnAcceptFailed] = Vector.empty,
      onSignSucceed: Vector[OnSignSucceed] = Vector.empty,
      onSignFailed: Vector[OnSignFailed] = Vector.empty
  ): DLCNodeCallbacks = {
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
          onPeerConnectionFailed),
      onOfferSendSucceed =
        CallbackHandler[Sha256Digest, OnOfferSendSucceed]("onOfferSendSucceed",
                                                          onOfferSendSucceed),
      onOfferSendFailed =
        CallbackHandler[(Sha256Digest, String), OnOfferSendFailed](
          "onOfferSendFailed",
          onOfferSendFailed),
      onAcceptSucceed =
        CallbackHandler[Sha256Digest, OnAcceptSucceed]("onAcceptSucceed",
                                                       onAcceptSucceed),
      onAcceptFailed = CallbackHandler[(Sha256Digest, String), OnAcceptFailed](
        "onAcceptFailed",
        onAcceptFailed),
      onSignSucceed =
        CallbackHandler[Sha256Digest, OnSignSucceed]("onSignSucceed",
                                                     onSignSucceed),
      onSignFailed =
        CallbackHandler[(Sha256Digest, String), OnSignFailed]("onSignFailed",
                                                              onSignFailed)
    )
  }
}
