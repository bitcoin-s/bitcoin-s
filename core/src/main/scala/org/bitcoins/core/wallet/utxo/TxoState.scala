package org.bitcoins.core.wallet.utxo

import org.bitcoins.crypto.StringFactory

/** Represents the various states a transaction output can be in */
sealed abstract class TxoState

sealed abstract class ReceivedState extends TxoState

sealed abstract class SpentState extends TxoState

object TxoState extends StringFactory[TxoState] {

  /** Means that no funds have been sent to this utxo EVER */
  final case object DoesNotExist extends TxoState

  /** A coinbase output that has not reached maturity and cannot be spent yet.
    * https://bitcoin.stackexchange.com/questions/1991/what-is-the-block-maturation-time
    */
  final case object ImmatureCoinbase extends TxoState

  /** Means we have received funds to this utxo, and they have not been confirmed in a block */
  final case object BroadcastReceived extends ReceivedState

  /** Means we have received funds to this utxo, and they have some confirmations but
    * have not reached our confirmation threshold
    */
  final case object PendingConfirmationsReceived extends ReceivedState

  /** Means we have received funds and they are fully confirmed for this utxo */
  final case object ConfirmedReceived extends ReceivedState

  /** Means we have not spent this utxo yet, but will be used in a future transaction */
  final case object Reserved extends SpentState

  /** Means we have spent this utxo, and they have not been confirmed in a block */
  final case object BroadcastSpent extends SpentState

  /** Means we have spent this utxo, and they have some confirmations but
    * have not reached our confirmation threshold
    */
  final case object PendingConfirmationsSpent extends SpentState

  /** Means we have spent this utxo, and it is fully confirmed */
  final case object ConfirmedSpent extends SpentState

  val pendingConfStates: Set[TxoState] =
    Set(BroadcastSpent,
        BroadcastReceived,
        ImmatureCoinbase,
        PendingConfirmationsReceived,
        PendingConfirmationsSpent)

  val confirmedStates: Set[TxoState] =
    Set(TxoState.ConfirmedReceived, TxoState.ConfirmedSpent)

  val receivedStates: Set[TxoState] =
    Set(PendingConfirmationsReceived, ConfirmedReceived, BroadcastReceived)

  val spentStates: Set[TxoState] =
    Set(PendingConfirmationsSpent,
        TxoState.ConfirmedSpent,
        Reserved,
        BroadcastSpent)

  val broadcastStates: Set[TxoState] = Set(BroadcastReceived, BroadcastSpent)

  val all: Vector[TxoState] = Vector(
    DoesNotExist,
    ImmatureCoinbase,
    BroadcastReceived,
    PendingConfirmationsReceived,
    ConfirmedReceived,
    Reserved,
    BroadcastSpent,
    PendingConfirmationsSpent,
    ConfirmedSpent
  )

  override def fromStringOpt(str: String): Option[TxoState] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): TxoState = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find txo state for string=${string}")
    }
  }
}
