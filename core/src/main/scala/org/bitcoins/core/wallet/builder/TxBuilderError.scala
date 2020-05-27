package org.bitcoins.core.wallet.builder

import org.bitcoins.core.protocol.transaction.TransactionOutput

import scala.util.Failure

/**
  * Represents an error that can be returned by the [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]]
  * if it failed to sign a set of utxos
  */
sealed abstract class TxBuilderError

object TxBuilderError {

  /**
    * This error indicates that the transaction failed to pass the invariants the user wanted to hold
    * true after the signing process was complete. An example of this is the transaction is too big,
    * or the fee level was too high or low.
    */
  val FailedUserInvariants = Failure(
    new IllegalArgumentException(
      "This tx fails the invariants function you passed in"))

  /**
    * Means that we gave too many [[org.bitcoins.crypto.Sign Sign]] for the TxBuilder
    * to use during the signing process for a utxo.
    * An example of this occurring is if we gave 2 private keys to sign a p2pkh spk.
    * A p2pkh only requires one private key to sign the utxo.
    */
  val TooManySigners = Failure(
    new IllegalArgumentException(
      "You passed in too many signers for this scriptPubKey type"))

  /**
    * Means that you are using the wrong [[org.bitcoins.core.wallet.signer.Signer Signer]] to
    * sign the given [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    */
  val WrongSigner = Failure(new IllegalArgumentException(
    "You did not pass in the write Signer to sign the given transaction, you probably gave the wrong identifier"))

  /**
    * Means that the [[org.bitcoins.core.protocol.script.ScriptWitnessV0 ScriptWitnessV0]] you passed as an argument does
    * not hash to the commitment inside of [[org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0 P2WSHWitnessSPKV0]]
    */
  val WrongWitness = Failure(new IllegalArgumentException(
    "You passed in the wrong ScriptWitness type to sign the given WitnessScriptPubKey"))

  /**
    * Means that the redeem script you passed as an argument does not hash to the commitment
    * inside of the [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]]
    */
  val WrongRedeemScript = Failure(new IllegalArgumentException(
    "Means that the redeem script you passed as an argument does not hash to the commitment"))

  /**
    * Means that you passed the wrong public key for a
    * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey P2PKHScriptPubKey]] or a
    * [[org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0 P2WPKHWitnessSPKV0]] that you are trying to spend
    */
  //case object WrongPublicKey extends TxBuilderError
  val WrongPublicKey = Failure(
    new IllegalArgumentException(
      "You passed in the wrong public key to sign a P2PKHScriptPubKey"))

  /**
    * Can occurr when we are trying to sign a
    * [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]] but
    * we do not have a redeem script for that p2sh spk.
    */
  //case object NoRedeemScript extends TxBuilderError
  val NoRedeemScript = Failure(
    new IllegalArgumentException(
      "We are missing a redeem script to sign a transaction"))

  /**
    * Can occurr when we are trying to sign a
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    * but we do not have a [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]] for that witness spk
    */
  //case object NoWitness extends TxBuilderError
  val NoWitness = Failure(
    new IllegalArgumentException("We are missing a witness redeem script"))

  /** We expected a
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 WitnessScriptPubKeyV0]],
    * but got a non witness spk type */
  val NonWitnessSPK = Failure(
    new IllegalArgumentException(
      "We expected a witness spk, but got a non witness spk"))

  /** We cannot have a
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]] nested inside of another
    * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] */
  //case object NestedWitnessSPK extends TxBuilderError
  val NestedWitnessSPK = Failure(
    new IllegalArgumentException("We cannot nested witness SPKs"))

  /** We cannot have a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]]
    * nested inside of another spk   */
  val NestedP2SHSPK = Failure(
    new IllegalArgumentException("We cannot sign nested P2SHScriptPubKeys"))

  /**
    * Means that there is no signer defined for the given
    * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] type.
    * An example of a spk with no signer that is defined is
    * [[org.bitcoins.core.protocol.script.WitnessCommitment WitnessCommitment]]
    */
  val NoSigner = Failure(
    new IllegalArgumentException(
      "We have no means to sign the given scriptpubkey"))

  /**
    * Means that you specified a fee that was too large for the change output you provided.
    * This may happen if you have a transaction with a lot of inputs, or the change output you provided
    * is a output that contains a very small amount of bitcoin.
    */
  val FeeToLarge = Failure(new IllegalArgumentException("Fee too large"))

  /**
    * Means that the
    * [[org.bitcoins.core.wallet.builder.TxBuilder.destinations TxBuilder.destinations]]
    * outputs you specified when creating the [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]] are NOT
    * all included in the final signed tx
    */
  val MissingDestinationOutput = Failure(
    new IllegalArgumentException(
      "Lost a transaction output in the finalizing process"))

  /**
    * Means that the script we are signing for requires a public key, but we did not pass one in
    * as a parameter inside of [[org.bitcoins.crypto.Sign Sign]]
    */
  val MissingPublicKey = Failure(
    new IllegalArgumentException(
      "You did not pass in a public key to sign a P2PKHScriptPubKey with"))

  //case object MissingOutPoint extends TxBuilderError
  val MissingOutPoint = Failure(
    new IllegalArgumentException("We cannot this outpoint in the utxo map"))

  /**
    * Means that the signed version of this transaction has MORE outputs than what was specified
    * when building the [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]].
    * [[org.bitcoins.core.wallet.builder.TxBuilder.destinations TxBuilder.destinations]] &&
    * [[org.bitcoins.core.wallet.builder.TxBuilder.changeSPK TxBuilder.changeSPK]] should
    * be the only outputs in the signedTx
    */
  val ExtraOutputsAdded = Failure(new IllegalArgumentException(
    "More outputs were added to the transaction than were initally passed in"))

  /**
    * Means that the transaction spends outpoints that were not given when creating
    * the [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]], aka, we should
    * only spend outpoints in
    * [[org.bitcoins.core.wallet.builder.TxBuilder.outPoints TxBuilder.outPoints]]
    */
  val ExtraOutPoints = Failure(new IllegalArgumentException(
    "Means that the transaction spends outpoints that were not given when creating the TxBuilder"))

  /** Means that this transaction attempts to print satoshis out of thin air */
  val MintsMoney = Failure(new IllegalArgumentException(
    "This transaction creates spends more money than it was funded by the given utxos"))

  /** Means that the fee was too low for
    * [[org.bitcoins.core.wallet.builder.TxBuilder.feeRate TxBuilder.feeRate]] */
  val LowFee = Failure(
    new IllegalArgumentException("Means that the fee was too low"))

  /** Means tha this transaction pays too high of a fee for
    * [[org.bitcoins.core.wallet.builder.TxBuilder.feeRate TxBuilder.feeRate]] */

  val HighFee = Failure(
    new IllegalArgumentException("Means that the fee was too high"))

  /**
    * Indicates we are spending multiple
    * [[org.bitcoins.core.protocol.script.CLTVScriptPubKey CLTVScriptPubKey]],
    * and that one of those spk's outputs are locked by block height, while the other is locked by
    * a time stamp. Since there is only one locktime field on a transaction, we cannot satisfy both of these
    * locktimes simultaneously.
    */
  val IncompatibleLockTimes = Failure(new IllegalArgumentException(
    "Means you tried to spend an output that requires a lock by blockheight, and another output that requires a lock by timestamp"))

  /** Means we have a output on this transaction below
    * [[org.bitcoins.core.policy.Policy.dustThreshold Policy.dustThreshold]] */
  def OutputBelowDustThreshold(
      belowDustOutputs: Seq[TransactionOutput]): Failure[Nothing] =
    Failure(new IllegalArgumentException(
      s"The p2p network discourages outputs below the dustThreshold, $belowDustOutputs, this tx won't be relayed"))

  val UnknownError = Failure(new IllegalArgumentException)
}
