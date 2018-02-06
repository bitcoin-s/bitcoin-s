package org.bitcoins.core.wallet.builder

sealed abstract class TxBuilderError




object TxBuilderError {
  /** This error indicates that the transaction failed to pass the invariants the user wanted to hold
    * true after the signing process was complete. An example of this is the transaction is too big,
    * or the fee level was too high or low.
    */
  case object FailedUserInvariants extends TxBuilderError

  /** Means that we gave too many keys for the TxBuilder to use during the signing process for a
    * utxo. An example of this occurring is if we gave 2 private keys to sign a p2pkh spk.
    * A p2pkh only requires one private key to sign the utxo.
    */
  case object TooManyKeys extends TxBuilderError

}