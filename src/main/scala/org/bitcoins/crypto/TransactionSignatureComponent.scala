package org.bitcoins.crypto

import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.protocol.transaction.Transaction
import org.bitcoins.script.flag.ScriptFlag

/**
 * Created by chris on 4/6/16.
 * Represents a transaction whose input is being checked against the spending conditions of the
 * scriptPubKey
 */
trait TransactionSignatureComponent {

  /**
   * The transaction being checked for the validity of signatures
 *
   * @return
   */
  def transaction : Transaction

  /**
   * The index of the input whose script signature is being checked
 *
   * @return
   */
  def inputIndex : Int

  /**
   * The script signature being checked
 *
   * @return
   */
  def scriptSignature = transaction.inputs(inputIndex).scriptSignature
  /**
   * The scriptPubKey for which the input is being checked against
 *
   * @return
   */
  def scriptPubKey : ScriptPubKey

  /**
   * The flags that are needed to verify if the signature is correct
 *
   * @return
   */
  def flags : Seq[ScriptFlag]
}
