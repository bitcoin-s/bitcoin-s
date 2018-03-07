package org.bitcoins.core.wallet.builder

/** Represents an error that can be returned by the [[org.bitcoins.core.wallet.builder.TxBuilder]]
  * if it failed to sign a set of utxos
  */
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

  /** Means that you are using the wrong [[org.bitcoins.core.wallet.signer.Signer]] to
    * sign the given [[org.bitcoins.core.protocol.script.ScriptPubKey]]
    */
  case object WrongSigner extends TxBuilderError

  /** Can occurr when we are trying to sign a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] but
    * we do not have a redeem script for that p2sh spk.
    */
  case object NoRedeemScript extends TxBuilderError

  /** Can occurr when we are trying to sign a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]]
    * but we do not have a [[org.bitcoins.core.protocol.script.ScriptWitness]] for that witness spk
    */
  case object NoWitness extends TxBuilderError

  /** We expected a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0]], but got a non witness spk type */
  case object NonWitnessSPK extends TxBuilderError

  /** We cannot have a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]] nested inside of another [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]] */
  case object NestedWitnessSPK extends TxBuilderError

  /** We cannot have a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] nested inside of another spk   */
  case object NestedP2SHSPK extends TxBuilderError

  /** We cannot have a [[org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0]] nested inside of another spk */
  case object NestedP2WSHSPK extends TxBuilderError

  /** Means that there is no signer defined for the given [[org.bitcoins.core.protocol.script.ScriptPubKey]] type.
    * An example of a spk with no signer that is defined is [[org.bitcoins.core.protocol.script.WitnessCommitment]]
    */
  case object NoSigner extends TxBuilderError

  /** Means that you specified a fee that was too large for the change output you provided.
    * This may happen if you have a transaction with a lot of inputs, or the change output you provided
    * is a output that contains a very small amount of bitcoin.
    * */
  case object FeeToLarge extends TxBuilderError

  /** Means that the [[TxBuilder.destinations]] outputs you specified when creating the [[TxBuilder]] are NOT
    * all included in the final signed tx
    */
  case object MissingDestinationOutput extends TxBuilderError

  /** Means that you provided a outpoint in the [[TxBuilder.utxoMap]] that does not
    * exist inside of [[TxBuilder.creditingTxs]]. You cannot spend an outpoint that was not
    * passed into the txbuilder originally
    */
  case object MissingCreditingTx extends TxBuilderError

  /** Means that the script we are signing for requires a public key, but we did not pass one in
    * as a parameter inside of [[org.bitcoins.core.wallet.signer.Signer.Sign]]
    */
  case object MissingPublicKey extends TxBuilderError

  /** Means that the signed version of this transaction has MORE outputs than what was specified
    * when building the [[TxBuilder]]. [[TxBuilder.destinations]] && [[TxBuilder.changeOutput]] should
    * be the only outputs in the signedTx
    */
  case object ExtraOutputsAdded extends TxBuilderError

  /** Means that the transaction spends outpoints that were not given when creating
    * the [[TxBuilder]], aka, we should only spend outpoints in [[TxBuilder.outPoints]]
    */
  case object ExtraOutPoints extends TxBuilderError

  /** Means that this transaction attempts to print satoshis out of thin air */
  case object MintsMoney extends TxBuilderError

  /** Means that the fee was too low for [[TxBuilder.feeRate]] */
  case object LowFee extends TxBuilderError

  /** Means tha this transaction pays too high of a fee for [[TxBuilder.feeRate]] */
  case object HighFee extends TxBuilderError

}