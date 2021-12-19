package org.bitcoins.dlc.wallet

import grizzled.slf4j.Logger
import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.dlc.models.DLCStatus

import scala.concurrent.{ExecutionContext, Future}

case class DLCWalletCallbacks(
    onStateChange: CallbackHandler[DLCStatus, OnDLCStateChange]) {

  def executeOnDLCStateChange(logger: Logger, status: DLCStatus)(implicit
      ec: ExecutionContext): Future[Unit] = {
    onStateChange.execute(
      status,
      (err: Throwable) =>
        logger.error(s"${onStateChange.name} Callback failed with error: ",
                     err))
  }

  def +(other: DLCWalletCallbacks): DLCWalletCallbacks = {
    copy(onStateChange = onStateChange ++ other.onStateChange)
  }
}

object DLCWalletCallbacks {

  val empty: DLCWalletCallbacks = {
    DLCWalletCallbacks(
      CallbackHandler[DLCStatus, OnDLCStateChange]("onDLCStateChange",
                                                   Vector.empty))
  }

  def onDLCStateChange(f: OnDLCStateChange): DLCWalletCallbacks = {
    val handler =
      CallbackHandler[DLCStatus, OnDLCStateChange]("onDLCStateChange",
                                                   Vector(f))
    DLCWalletCallbacks(handler)
  }

}

/** Triggered when [[DLCStatus.state]] is changed in the database */
trait OnDLCStateChange extends Callback[DLCStatus]
