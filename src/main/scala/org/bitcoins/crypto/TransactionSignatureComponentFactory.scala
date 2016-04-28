package org.bitcoins.crypto

import org.bitcoins.protocol.script.{P2SHScriptSignature, P2SHScriptPubKey, ScriptPubKey}
import org.bitcoins.protocol.transaction.Transaction
import org.bitcoins.script.flag.ScriptFlag

/**
 * Created by chris on 4/6/16.
 */
trait TransactionSignatureComponentFactory {

  private sealed case class TransactionSignatureComponentImpl(transaction : Transaction, inputIndex : Int,
    scriptPubKey : ScriptPubKey, flags : Seq[ScriptFlag]) extends TransactionSignatureComponent

  def factory(transaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
               flags : Seq[ScriptFlag]) : TransactionSignatureComponent = {
    TransactionSignatureComponentImpl(transaction,inputIndex,scriptPubKey, flags)
  }

  /**
   * This factory method is used for changing the scriptPubKey inside of a txSignatureComponent
 *
   * @param oldTxSignatureComponent
   * @param scriptPubKey
   * @return
   */
  def factory(oldTxSignatureComponent : TransactionSignatureComponent, scriptPubKey : ScriptPubKey) : TransactionSignatureComponent = {
    TransactionSignatureComponentImpl(oldTxSignatureComponent.transaction,
      oldTxSignatureComponent.inputIndex,scriptPubKey, oldTxSignatureComponent.flags)
  }

}

object TransactionSignatureComponentFactory extends TransactionSignatureComponentFactory