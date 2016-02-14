package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionInputParser, TransactionElement}
import org.scalacoin.protocol.VarInt
import org.scalacoin.protocol.script.ScriptSignature

/**
 * Created by chris on 12/26/15.
 */
trait TransactionInput extends TransactionElement {
  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long

  def scriptSigVarInt : VarInt
  //https://bitcoin.org/en/developer-reference#txin
  def size = previousOutput.size + scriptSignature.size + scriptSigVarInt.size.toInt + 4

  def hex = RawTransactionInputParser.write(Seq(this))
}

case class TransactionInputImpl(previousOutput : TransactionOutPoint, scriptSigVarInt : VarInt,
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput
