package org.bitcoins.wallet

import org.bitcoins.core.api.{Callback, CallbackHandler}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.wallet.models.{AddressDb, SpendingInfoDb}
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}

/**
  * Callbacks for responding to events in the wallet.
  * The appropriate callback is executed whenever the wallet finishes,
  * the corresponding function.
  */
trait WalletCallbacks {

  def onTransactionProcessed: CallbackHandler[
    Transaction,
    OnTransactionProcessed]

  def onTransactionBroadcast: CallbackHandler[
    Transaction,
    OnTransactionBroadcast]
  def onReservedUtxos: CallbackHandler[Vector[SpendingInfoDb], OnReservedUtxos]
  def onNewAddressGenerated: CallbackHandler[AddressDb, OnNewAddressGenerated]

  def +(other: WalletCallbacks): WalletCallbacks

  def executeOnTransactionProcessed(logger: Logger, tx: Transaction)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onTransactionProcessed.execute(logger, tx)
  }

  def executeOnTransactionBroadcast(logger: Logger, tx: Transaction)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onTransactionBroadcast.execute(logger, tx)
  }

  def executeOnReservedUtxos(logger: Logger, utxos: Vector[SpendingInfoDb])(
      implicit ec: ExecutionContext): Future[Unit] = {
    onReservedUtxos.execute(logger, utxos)
  }

  def executeOnNewAddressGenerated(logger: Logger, addressDb: AddressDb)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onNewAddressGenerated.execute(logger, addressDb)
  }

}

/** Callback for handling a processed transaction */
trait OnTransactionProcessed extends Callback[Transaction]

trait OnTransactionBroadcast extends Callback[Transaction]

trait OnReservedUtxos extends Callback[Vector[SpendingInfoDb]]

trait OnNewAddressGenerated extends Callback[AddressDb]

object WalletCallbacks {

  private case class WalletCallbacksImpl(
      onTransactionProcessed: CallbackHandler[
        Transaction,
        OnTransactionProcessed],
      onTransactionBroadcast: CallbackHandler[
        Transaction,
        OnTransactionBroadcast],
      onReservedUtxos: CallbackHandler[Vector[SpendingInfoDb], OnReservedUtxos],
      onNewAddressGenerated: CallbackHandler[AddressDb, OnNewAddressGenerated]
  ) extends WalletCallbacks {
    override def +(other: WalletCallbacks): WalletCallbacks = copy(
      onTransactionProcessed = onTransactionProcessed ++ other.onTransactionProcessed,
      onTransactionBroadcast = onTransactionBroadcast ++ other.onTransactionBroadcast,
      onReservedUtxos = onReservedUtxos ++ other.onReservedUtxos,
      onNewAddressGenerated = onNewAddressGenerated ++ other.onNewAddressGenerated
    )
  }

  /** Does nothing */
  def noop[T]: T => Future[Unit] = _ => FutureUtil.unit

  /** Constructs a set of callbacks that only acts on processed transaction */
  def onTransactionProcessed(f: OnTransactionProcessed): WalletCallbacks =
    WalletCallbacks(onTransactionProcessed = Vector(f))

  /** Constructs a set of callbacks that only acts on broadcasted transaction */
  def onTransactionBroadcast(f: OnTransactionBroadcast): WalletCallbacks =
    WalletCallbacks(onTransactionBroadcast = Vector(f))

  /** Constructs a set of callbacks that only acts on utxos becoming reserved or unreserved */
  def onReservedUtxos(f: OnReservedUtxos): WalletCallbacks =
    WalletCallbacks(onReservedUtxos = Vector(f))

  /** Constructs a set of callbacks that only acts on new address generation */
  def onNewAddressGenerated(f: OnNewAddressGenerated): WalletCallbacks =
    WalletCallbacks(onNewAddressGenerated = Vector(f))

  /** Empty callbacks that does nothing with the received data */
  val empty: WalletCallbacks =
    apply(Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  def apply(
      onTransactionProcessed: Vector[OnTransactionProcessed] = Vector.empty,
      onTransactionBroadcast: Vector[OnTransactionBroadcast] = Vector.empty,
      onReservedUtxos: Vector[OnReservedUtxos] = Vector.empty,
      onNewAddressGenerated: Vector[OnNewAddressGenerated] = Vector.empty
  ): WalletCallbacks = {
    WalletCallbacksImpl(
      onTransactionProcessed =
        CallbackHandler[Transaction, OnTransactionProcessed](
          "onTransactionProcessed",
          onTransactionProcessed),
      onTransactionBroadcast =
        CallbackHandler[Transaction, OnTransactionBroadcast](
          "onTransactionBroadcast",
          onTransactionBroadcast),
      onReservedUtxos =
        CallbackHandler[Vector[SpendingInfoDb], OnReservedUtxos](
          "onReservedUtxos",
          onReservedUtxos),
      onNewAddressGenerated = CallbackHandler[AddressDb, OnNewAddressGenerated](
        "onNewAddressGenerated",
        onNewAddressGenerated)
    )
  }
}
