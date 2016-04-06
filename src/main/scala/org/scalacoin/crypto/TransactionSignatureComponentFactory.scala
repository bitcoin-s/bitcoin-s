package org.scalacoin.crypto

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.flag.ScriptFlag

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

}

object TransactionSignatureComponentFactory extends TransactionSignatureComponentFactory