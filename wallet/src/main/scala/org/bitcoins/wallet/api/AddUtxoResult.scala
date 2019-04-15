package org.bitcoins.wallet.api

sealed trait AddUtxoResult {
  def flatMap(f: AddUtxoResult => AddUtxoResult) = ???
  def map(success: AddUtxoSuccess => AddUtxoResult) = ???
}

case class AddUtxoSuccess(walletApi: WalletApi) extends AddUtxoResult

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
