package org.scalacoin.protocol.transaction

import org.scalacoin.protocol.VarIntImpl
import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 2/19/16.
 */
trait TransactionInputFactory { this : TransactionInput =>

  def factory(scriptSig : ScriptSignature) : TransactionInput = {
    TransactionInputImpl(previousOutput,ScalacoinUtil.parseVarInt(scriptSignature),scriptSig,sequence)
  }

  def factory(sequenceNumber : Long) : TransactionInput = {
    TransactionInputImpl(previousOutput, scriptSigVarInt,scriptSignature,sequenceNumber)
  }

}
