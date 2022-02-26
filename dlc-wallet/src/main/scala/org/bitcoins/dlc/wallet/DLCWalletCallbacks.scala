package org.bitcoins.dlc.wallet

import grizzled.slf4j.Logger
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.crypto.Sha256Digest

import scala.concurrent.{ExecutionContext, Future}

case class DLCWalletCallbacks(
    onStateChange: CallbackHandler[DLCStatus, OnDLCStateChange],
    onOfferAdd: CallbackHandler[IncomingDLCOfferDb, OnDLCOfferAdd],
    onOfferRemove: CallbackHandler[Sha256Digest, OnDLCOfferRemove]) {

  def executeOnDLCStateChange(logger: Logger, status: DLCStatus)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onStateChange.execute(
      status,
      (err: Throwable) =>
        logger.error(s"${onStateChange.name} Callback failed with error: ",
                     err))
  }

  def executeOnDLCOfferAdd(logger: Logger, offerDb: IncomingDLCOfferDb)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onOfferAdd.execute(
      offerDb,
      (err: Throwable) =>
        logger.error(s"${onStateChange.name} Callback failed with error: ",
                     err))
  }

  def executeOnDLCOfferRemove(logger: Logger, offerHash: Sha256Digest)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onOfferRemove.execute(
      offerHash,
      (err: Throwable) =>
        logger.error(s"${onStateChange.name} Callback failed with error: ",
                     err))
  }

  def +(other: DLCWalletCallbacks): DLCWalletCallbacks = {
    copy(onStateChange = onStateChange ++ other.onStateChange,
         onOfferAdd = onOfferAdd ++ other.onOfferAdd,
         onOfferRemove = onOfferRemove ++ other.onOfferRemove)
  }
}

object DLCWalletCallbacks {

  val empty: DLCWalletCallbacks = {
    DLCWalletCallbacks(
      CallbackHandler[DLCStatus, OnDLCStateChange]("onDLCStateChange",
                                                   Vector.empty),
      CallbackHandler[IncomingDLCOfferDb, OnDLCOfferAdd]("onDLCOfferAdd",
                                                         Vector.empty),
      CallbackHandler[Sha256Digest, OnDLCOfferRemove]("onDLCOfferRemove",
                                                      Vector.empty)
    )
  }

  def onDLCStateChange(f: OnDLCStateChange): DLCWalletCallbacks = {
    val handler =
      CallbackHandler[DLCStatus, OnDLCStateChange]("onDLCStateChange",
                                                   Vector(f))
    empty.copy(onStateChange = handler)
  }

  def onDLCOfferAdd(f: OnDLCOfferAdd): DLCWalletCallbacks = {
    val handler =
      CallbackHandler[IncomingDLCOfferDb, OnDLCOfferAdd]("onDLCOfferAdd",
                                                         Vector(f))
    empty.copy(onOfferAdd = handler)
  }

  def onDLCOfferRemove(f: OnDLCOfferRemove): DLCWalletCallbacks = {
    val handler =
      CallbackHandler[Sha256Digest, OnDLCOfferRemove]("onDLCOfferRemove",
                                                      Vector(f))
    empty.copy(onOfferRemove = handler)
  }

}

/** Triggered when [[DLCStatus.state]] is changed in the database */
trait OnDLCStateChange extends Callback[DLCStatus]

trait OnDLCOfferAdd extends Callback[IncomingDLCOfferDb]

trait OnDLCOfferRemove extends Callback[Sha256Digest]
