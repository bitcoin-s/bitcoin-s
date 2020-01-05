package org.bitcoins.core.wallet.utxo

/** Represents the various states a transaction output can be in */
sealed abstract class TxoState

sealed abstract class ReceivedState extends TxoState

sealed abstract class SpentState extends TxoState

object TxoState {

  /** Means that no funds have been sent to this utxo EVER */
  final case object DoesNotExist extends TxoState

  /** Means we have received funds to this utxo, but they are not confirmed */
  final case object PendingConfirmationsReceived extends ReceivedState

  /** Means we have received funds and they are fully confirmed for this utxo */
  final case object ConfirmedReceived extends ReceivedState

  /** Means we have spent this utxo, but it is not fully confirmed */
  final case object PendingConfirmationsSpent extends SpentState

  /** Means we have spent this utxo, and it is fully confirmed */
  final case object ConfirmedSpent extends SpentState

  val all: Vector[TxoState] = Vector(DoesNotExist,
                                     PendingConfirmationsReceived,
                                     ConfirmedReceived,
                                     PendingConfirmationsSpent,
                                     ConfirmedSpent)

  def fromString(str: String): Option[TxoState] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }
}
