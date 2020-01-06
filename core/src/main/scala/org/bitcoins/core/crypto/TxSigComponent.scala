package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.flag.ScriptFlag

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 4/6/16.
  * Represents a transaction whose input is being checked against the spending conditions of a
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
  */
sealed abstract class TxSigComponent {

  /** The transaction being checked for the validity of signatures */
  def transaction: Transaction

  /** The index of the input whose script signature is being checked */
  def inputIndex: UInt32

  def input: TransactionInput = transaction.inputs(inputIndex.toInt)

  /** The script signature being checked */
  def scriptSignature: ScriptSignature = input.scriptSignature

  /** This is the output we are spending. We need this for script and digital signatures checks */
  def output: TransactionOutput

  /** The scriptPubKey for which the input is being checked against */
  def scriptPubKey: ScriptPubKey = output.scriptPubKey

  /** The amount of [[org.bitcoins.core.currency.CurrencyUnit CurrencyUnit]] we are spending in this TxSigComponent */
  def amount: CurrencyUnit = output.value

  /** The flags that are needed to verify if the signature is correct */
  def flags: Seq[ScriptFlag]

  /** Represents the serialization algorithm used to verify/create signatures for Bitcoin */
  def sigVersion: SignatureVersion
}

/**
  * The [[org.bitcoins.core.crypto.TxSigComponent TxSigComponent]]
  * used to evaluate the the original Satoshi transaction digest algorithm.
  * Basically this is every spk that is not a
  * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]] EXCEPT in the case of a
  * P2SH(witness script) [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
  */
sealed abstract class BaseTxSigComponent extends TxSigComponent {
  override def sigVersion: SignatureVersion = SigVersionBase
}

sealed abstract class P2SHTxSigComponent extends BaseTxSigComponent {

  require(
    input.scriptSignature
      .isInstanceOf[P2SHScriptSignature],
    "Must have P2SHScriptSignature for P2SH, got: " + input.scriptSignature
  )
  require(
    output.scriptPubKey
      .isInstanceOf[P2SHScriptPubKey],
    "Must have P2SHScriptPubKey for P2SH, got: " + output.scriptPubKey
  )

  override def scriptPubKey: P2SHScriptPubKey =
    output.scriptPubKey.asInstanceOf[P2SHScriptPubKey]

  override def scriptSignature: P2SHScriptSignature =
    input.scriptSignature.asInstanceOf[P2SHScriptSignature]
}

/**
  * The [[org.bitcoins.core.crypto.TxSigComponent TxSigComponent]]
  * used to represent all the components necessarily for
  * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki BIP143]].
  * Examples of these [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]'s
  * are [[org.bitcoins.core.protocol.script.P2WPKHWitnessV0 P2WPKHWitnessSPKV0]],
  * [[org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0  P2WSHWitnessSPKV0]],
  * and P2SH(witness script)
  */
sealed trait WitnessTxSigComponent extends TxSigComponent {

  override def transaction: WitnessTransaction

  def witness: ScriptWitness = transaction.witness.witnesses(inputIndex.toInt)

  def witnessVersion: WitnessVersion

  override def sigVersion: SignatureVersion = SigVersionWitnessV0
}

/** This represents checking the [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
  * against a [[org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0 P2WPKHWitnessSPKV0]] or a
  * [[org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0 P2WSHWitnessSPKV0]] */
sealed abstract class WitnessTxSigComponentRaw extends WitnessTxSigComponent {
  override def scriptPubKey: WitnessScriptPubKey =
    output.scriptPubKey.asInstanceOf[WitnessScriptPubKey]

  override def witnessVersion: WitnessVersion = {
    scriptPubKey.witnessVersion
  }
}

/** This represents checking the [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
  * against a P2SH(P2WSH) or P2SH(P2WPKH) scriptPubKey */
sealed abstract class WitnessTxSigComponentP2SH
    extends P2SHTxSigComponent
    with WitnessTxSigComponent {
  override def scriptPubKey: P2SHScriptPubKey =
    output.scriptPubKey.asInstanceOf[P2SHScriptPubKey]

  override def scriptSignature: P2SHScriptSignature = {
    val s = transaction.inputs(inputIndex.toInt).scriptSignature
    require(s.isInstanceOf[P2SHScriptSignature],
            "Must have P2SHScriptSignature for P2SH(WitSPK()), got: " + s)
    val p2sh = s.asInstanceOf[P2SHScriptSignature]
    p2sh

  }

  def witnessScriptPubKey: Try[WitnessScriptPubKey] =
    scriptSignature.redeemScript match {
      case w: WitnessScriptPubKey => Success(w)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2SHScriptPubKey | _: CSVScriptPubKey | _: CLTVScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | EmptyScriptPubKey) =>
        Failure(new IllegalArgumentException(
          "Must have a witness scriptPubKey as redeemScript for P2SHScriptPubKey in WitnessTxSigComponentP2SH, got: " + x))

    }

  override def witnessVersion: WitnessVersion = witnessScriptPubKey match {
    case Success(w)   => w.witnessVersion
    case Failure(err) => throw err
  }

  override def amount: CurrencyUnit = output.value

}

/**
  * This represents a 'rebuilt' [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
  * that was constructed from [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
  * After the
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] is rebuilt, we need to use that rebuilt
  * scriptpubkey to evaluate the [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]]
  * See
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program BIP141]]
  * for more info on rebuilding P2WSH and P2WPKH scriptpubkeys
  */
sealed abstract class WitnessTxSigComponentRebuilt extends TxSigComponent {
  override def transaction: WitnessTransaction

  override def scriptPubKey: ScriptPubKey = output.scriptPubKey

  /** The [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]] we used to
    * rebuild the scriptPubKey above */
  def witnessScriptPubKey: WitnessScriptPubKey

  override def sigVersion: SignatureVersion = SigVersionWitnessV0

  def witnessVersion: WitnessVersion = witnessScriptPubKey.witnessVersion

}

object BaseTxSigComponent {

  private case class BaseTxSigComponentImpl(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends BaseTxSigComponent

  def apply(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): BaseTxSigComponent = {
    BaseTxSigComponentImpl(transaction, inputIndex, output, flags)
  }

}

object P2SHTxSigComponent {

  private case class P2SHTxSigComponentImpl(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends P2SHTxSigComponent

  def apply(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): P2SHTxSigComponent = {
    P2SHTxSigComponentImpl(transaction, inputIndex, output, flags)
  }
}

object WitnessTxSigComponent {

  def apply(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): WitnessTxSigComponent =
    output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        WitnessTxSigComponentRaw(transaction, inputIndex, output, flags)
      case _: P2SHScriptPubKey =>
        WitnessTxSigComponentP2SH(transaction, inputIndex, output, flags)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: LockTimeScriptPubKey | _: ConditionalScriptPubKey |
          _: WitnessCommitment | _: NonStandardScriptPubKey |
          EmptyScriptPubKey) =>
        throw new IllegalArgumentException(
          s"Cannot create a WitnessTxSigComponent out of $x")
    }

}

object WitnessTxSigComponentRaw {

  private case class WitnessTxSigComponentRawImpl(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends WitnessTxSigComponentRaw

  def apply(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): WitnessTxSigComponentRaw = {
    output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        WitnessTxSigComponentRawImpl(transaction, inputIndex, output, flags)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2SHScriptPubKey | _: LockTimeScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | EmptyScriptPubKey) =>
        throw new IllegalArgumentException(
          s"Cannot create a WitnessTxSigComponentRaw with a spk of $x")
    }

  }
}

object WitnessTxSigComponentP2SH {

  private case class WitnessTxSigComponentP2SHImpl(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends WitnessTxSigComponentP2SH

  def apply(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): WitnessTxSigComponentP2SH = {
    output.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        WitnessTxSigComponentP2SHImpl(transaction, inputIndex, output, flags)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: LockTimeScriptPubKey | _: ConditionalScriptPubKey |
          _: NonStandardScriptPubKey | _: WitnessCommitment |
          _: WitnessScriptPubKey | EmptyScriptPubKey) =>
        throw new IllegalArgumentException(
          s"Cannot create a WitnessTxSigComponentP2SH with a spk of $x")
    }

  }
}

object WitnessTxSigComponentRebuilt {

  private case class WitnessTxSigComponentRebuiltImpl(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      witnessScriptPubKey: WitnessScriptPubKey,
      flags: Seq[ScriptFlag])
      extends WitnessTxSigComponentRebuilt

  def apply(
      wtx: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      witScriptPubKey: WitnessScriptPubKey,
      flags: Seq[ScriptFlag]): WitnessTxSigComponentRebuilt = {
    WitnessTxSigComponentRebuiltImpl(wtx,
                                     inputIndex,
                                     output,
                                     witScriptPubKey,
                                     flags)
  }
}
