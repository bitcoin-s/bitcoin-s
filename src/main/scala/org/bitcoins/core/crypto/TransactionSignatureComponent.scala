package org.bitcoins.core.crypto

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness, SignatureVersion}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.flag.ScriptFlag

/**
 * Created by chris on 4/6/16.
 * Represents a transaction whose input is being checked against the spending conditions of the
 * scriptPubKey
 */
trait TransactionSignatureComponent {

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

  /** The witness that should be used to evaluate the script inside of this program */
  def witness: Option[ScriptWitness]

  /** The digest algorithm used to serialized/hash a transaction for signature creation/verification */
  def sigVersion: SignatureVersion
}


object TransactionSignatureComponent {

  private sealed case class TransactionSignatureComponentImpl(transaction : Transaction, inputIndex : UInt32,
                                                              scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag],
                                                              witness : Option[ScriptWitness], sigVersion: SignatureVersion) extends TransactionSignatureComponent

  def apply(transaction : Transaction, inputIndex : UInt32, scriptPubKey : ScriptPubKey,
            flags : Seq[ScriptFlag], witness : Option[ScriptWitness], sigVersion: SignatureVersion) : TransactionSignatureComponent = {
    TransactionSignatureComponentImpl(transaction,inputIndex, scriptPubKey, flags, witness, sigVersion)
  }

  /**
    * This factory method is used for changing the scriptPubKey inside of a txSignatureComponent
    *
    * @param oldTxSignatureComponent
    * @param scriptPubKey
    * @return
    */
  def apply(oldTxSignatureComponent : TransactionSignatureComponent, scriptPubKey : ScriptPubKey) : TransactionSignatureComponent = {
    TransactionSignatureComponent(oldTxSignatureComponent.transaction,
      oldTxSignatureComponent.inputIndex,scriptPubKey, oldTxSignatureComponent.flags,
      oldTxSignatureComponent.witness, oldTxSignatureComponent.sigVersion)
  }

}