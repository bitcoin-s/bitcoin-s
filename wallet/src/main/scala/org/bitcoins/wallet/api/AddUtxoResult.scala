package org.bitcoins.wallet.api

import org.bitcoins.wallet.models._

sealed trait AddUtxoResult

/** Contains the freshly added UTXO */
case class AddUtxoSuccess(spendingInfo: SpendingInfoDb) extends AddUtxoResult

/** Represents an error that might occur when adding an UTXO to the wallet */
sealed trait AddUtxoError extends Error with AddUtxoResult

object AddUtxoError {

  /**
    * The provided vout index does not exist in the given transaction
    */
  final case object VoutIndexOutOfBounds
      extends Error("VoutIndexOutOfBounds")
      with AddUtxoError

  /**
    * We could not convert the found ScriptPubKey into an address
    */
  final case object BadSPK extends Error("BadScriptPubKey") with AddUtxoError

  /**
    * The address associated with the provided UTXO could not be found
    * in our DB of addresses
    */
  final case object AddressNotFound
      extends Error("AddressNotFound")
      with AddUtxoError
}
