package org.scalacoin.protocol.transaction

import org.scalacoin.protocol.VarIntImpl
import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 2/19/16.
 */
trait TransactionInputFactory { this : TransactionInput =>

  def factory(scriptSig : ScriptSignature) : TransactionInput = {
    //need to calculate the new ScriptVarInt
    TransactionInputImpl(previousOutput,VarIntImpl(-1,-1),scriptSig,sequence)
  }

}
