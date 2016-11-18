package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.flag.ScriptFlag

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
sealed trait WitnessV0TransactionSignatureComponent extends TransactionSignatureComponent {
  def witness: ScriptWitness

  /** The amount of [[CurrencyUnit]] this input is spending */
  def amount: CurrencyUnit

  /** The digest algorithm used to serialized/hash a transaction for signature creation/verification */
  override def sigVersion: SignatureVersion = SigVersionWitnessV0


}


object TransactionSignatureComponent {

  private case class BaseTransactionSignatureComponentImpl(transaction : Transaction, inputIndex : UInt32,
                                                       scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag]) extends BaseTransactionSignatureComponent

  private case class WitnessV0TransactionSignatureComponentImpl(transaction : Transaction, inputIndex : UInt32,
                                                                scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag],
                                                                witness: ScriptWitness, amount: CurrencyUnit) extends WitnessV0TransactionSignatureComponent

  def apply(transaction : Transaction, inputIndex : UInt32, scriptPubKey : ScriptPubKey,
            flags : Seq[ScriptFlag], witness : ScriptWitness,
            amount: CurrencyUnit) : WitnessV0TransactionSignatureComponent = {
    WitnessV0TransactionSignatureComponentImpl(transaction,inputIndex, scriptPubKey, flags, witness, amount)
  }

  def apply(transaction : Transaction, inputIndex : UInt32,
            scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag]): BaseTransactionSignatureComponent = {
    BaseTransactionSignatureComponentImpl(transaction,inputIndex,scriptPubKey,flags)
  }

  /**
    * This factory method is used for changing the scriptPubKey inside of a txSignatureComponent
    *
    * @param oldTxSignatureComponent
    * @param scriptPubKey
    * @return
    */
  def apply(oldTxSignatureComponent : TransactionSignatureComponent, scriptPubKey : ScriptPubKey) : TransactionSignatureComponent = oldTxSignatureComponent match {
    case base: BaseTransactionSignatureComponent =>
    TransactionSignatureComponent(base.transaction,
      base.inputIndex,scriptPubKey, base.flags)
    case w: WitnessV0TransactionSignatureComponent =>
      TransactionSignatureComponent(w.transaction,w.inputIndex,w.scriptPubKey,w.flags,w.witness,w.amount)
  }

}