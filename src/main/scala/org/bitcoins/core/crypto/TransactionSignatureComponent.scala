package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.script.flag.ScriptFlag

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 4/6/16.
 * Represents a transaction whose input is being checked against the spending conditions of the
 * scriptPubKey
 */
sealed trait TransactionSignatureComponent {

  /** The transaction being checked for the validity of signatures */
  def transaction : Transaction

  /** The index of the input whose script signature is being checked */
  def inputIndex : UInt32

  /** The script signature being checked */
  def scriptSignature = transaction.inputs(inputIndex.toInt).scriptSignature

  /** The scriptPubKey for which the input is being checked against */
  def scriptPubKey : ScriptPubKey

  /** The flags that are needed to verify if the signature is correct*/
  def flags : Seq[ScriptFlag]

  /** Represents the serialization algorithm used to verify/create signatures for Bitcoin */
  def sigVersion: SignatureVersion
}

/** The [[TransactionSignatureComponent]] used to evaluate the the original Satoshi transaction digest algorithm */
sealed trait BaseTransactionSignatureComponent extends TransactionSignatureComponent {
  override def sigVersion = SigVersionBase
}

/** The [[TransactionSignatureComponent]] used to represent all the components necessarily for BIP143
  * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]]
  */
sealed trait WitnessTxSigComponent extends TransactionSignatureComponent {

  override def transaction: WitnessTransaction

  def witness: ScriptWitness = transaction.witness.witnesses(inputIndex.toInt)

  /** The amount of [[CurrencyUnit]] this input is spending */
  def amount: CurrencyUnit

  def witnessVersion: WitnessVersion
}

sealed trait WitnessTxSigComponentRaw extends WitnessTxSigComponent {
  override def scriptPubKey: WitnessScriptPubKey

  override def witnessVersion: WitnessVersion = scriptPubKey.witnessVersion

  override def sigVersion = SigVersionWitnessV0
}

sealed trait WitnessTxSigComponentP2SH extends WitnessTxSigComponent {
  override def scriptPubKey: P2SHScriptPubKey

  override def scriptSignature: P2SHScriptSignature = {
    val s = transaction.inputs(inputIndex.toInt).scriptSignature
    require(s.isInstanceOf[P2SHScriptSignature], "Must have P2SHScriptSignature for P2SH(P2WSH())")
    s.asInstanceOf[P2SHScriptSignature]
  }

  def witnessScriptPubKey: Try[WitnessScriptPubKey] = scriptSignature.redeemScript match {
    case w: WitnessScriptPubKey => Success(w)
    case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
         | _: CSVScriptPubKey | _: CLTVScriptPubKey | _: NonStandardScriptPubKey
         | _: WitnessCommitment | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Must have a witness scriptPubKey as redeemScript for P2SHScriptPubKey in WitnessTxSigComponentP2SH, got: " + x))

  }
  override def witnessVersion: WitnessVersion = witnessScriptPubKey match {
    case Success(w) => w.witnessVersion
    case Failure(err) => throw err
  }

  override def sigVersion = SigVersionWitnessV0
}

sealed trait WitnessTxSigComponentRebuilt extends TransactionSignatureComponent {
  override def scriptPubKey: ScriptPubKey

  def witnessScriptPubKey: WitnessScriptPubKey

  override def sigVersion = SigVersionWitnessV0

  def witnessVersion = witnessScriptPubKey.witnessVersion

  def amount: CurrencyUnit
}
object TransactionSignatureComponent {

  private case class BaseTransactionSignatureComponentImpl(transaction : Transaction, inputIndex : UInt32,
                                                       scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag]) extends BaseTransactionSignatureComponent


  def apply(transaction : Transaction, inputIndex : UInt32,
            scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag]): BaseTransactionSignatureComponent = {
    BaseTransactionSignatureComponentImpl(transaction,inputIndex,scriptPubKey,flags)
  }

}

object WitnessTxSigComponent {

  def apply(transaction: WitnessTransaction, inputIndex: UInt32, scriptPubKey: WitnessScriptPubKey,
            flags: Seq[ScriptFlag], amount: CurrencyUnit): WitnessTxSigComponent = {
    WitnessTxSigComponentRaw(transaction,inputIndex, scriptPubKey, flags, amount)
  }

  def apply(transaction : WitnessTransaction, inputIndex : UInt32, scriptPubKey : P2SHScriptPubKey,
            flags : Seq[ScriptFlag], amount: CurrencyUnit) : WitnessTxSigComponent = {
    WitnessTxSigComponentP2SH(transaction,inputIndex,scriptPubKey,flags,amount)
  }

}

object WitnessTxSigComponentRaw {
  private case class WitnessTxSigComponentRawImpl(transaction: WitnessTransaction,inputIndex: UInt32,
                                              scriptPubKey: WitnessScriptPubKey, flags: Seq[ScriptFlag],
                                                  amount: CurrencyUnit) extends WitnessTxSigComponentRaw

  def apply(transaction: WitnessTransaction,inputIndex: UInt32,
            scriptPubKey: WitnessScriptPubKey, flags: Seq[ScriptFlag], amount: CurrencyUnit): WitnessTxSigComponentRaw = {
    WitnessTxSigComponentRawImpl(transaction,inputIndex,scriptPubKey,flags,amount)
  }
}

object WitnessTxSigComponentP2SH {
  private case class WitnessTxSigComponentP2SHImpl(transaction: WitnessTransaction,inputIndex: UInt32,
                                                  scriptPubKey: P2SHScriptPubKey, flags: Seq[ScriptFlag],
                                                  amount: CurrencyUnit) extends WitnessTxSigComponentP2SH

  def apply(transaction: WitnessTransaction,inputIndex: UInt32, scriptPubKey: P2SHScriptPubKey, flags: Seq[ScriptFlag],
            amount: CurrencyUnit): WitnessTxSigComponentP2SH = {

    WitnessTxSigComponentP2SHImpl(transaction,inputIndex,scriptPubKey, flags, amount)
  }
}

object WitnessTxSigComponentRebuilt {
  private case class WitnessTxSigComponentRebuiltImpl(transaction: WitnessTransaction,inputIndex: UInt32,
                                                    scriptPubKey: ScriptPubKey, witnessScriptPubKey: WitnessScriptPubKey,
                                                    flags: Seq[ScriptFlag], amount: CurrencyUnit) extends WitnessTxSigComponentRebuilt

  def apply(wtx: WitnessTransaction, inputIndex: UInt32, scriptPubKey: ScriptPubKey, witScriptPubKey: WitnessScriptPubKey,
           flags: Seq[ScriptFlag], amount: CurrencyUnit): WitnessTxSigComponentRebuilt = {
    WitnessTxSigComponentRebuiltImpl(wtx,inputIndex,scriptPubKey,witScriptPubKey,flags,amount)
  }
}