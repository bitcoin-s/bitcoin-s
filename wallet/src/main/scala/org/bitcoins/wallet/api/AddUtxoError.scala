package org.bitcoins.wallet.api

/** Represents an error that might occur when adding an UTXO to the wallet */
sealed trait AddUtxoError extends Error

object AddUtxoError {

  /**
    * The provided vout index does not exist in the given transaction
    */
  case object VoutIndexOutOfBounds
      extends Error("VoutIndexOutOfBounds")
      with AddUtxoError

  /**
    * We could not convert the found ScriptPubKey into an address
    */
  case object BadSPK extends Error("BadScriptPubKey") with AddUtxoError

  /**
    * The address associated with the provided UTXO could not be found
    * in our DB of addresses
    */
  case object AddressNotFound extends Error("AddressNotFound") with AddUtxoError
}
