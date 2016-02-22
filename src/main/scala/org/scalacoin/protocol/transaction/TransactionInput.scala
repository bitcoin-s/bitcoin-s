package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionInputParser, TransactionElement}
import org.scalacoin.protocol.VarInt
import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignature}
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 12/26/15.
 */
trait TransactionInput extends TransactionElement with TransactionInputFactory {

  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long

  def scriptSigVarInt : VarInt //= ScalacoinUtil.parseVarInt(scriptSignature)
  //https://bitcoin.org/en/developer-reference#txin
  override def size = previousOutput.size + scriptSignature.size + scriptSigVarInt.size.toInt + 4

  def hex = RawTransactionInputParser.write(Seq(this))
}

object TransactionInput extends TransactionInput {
  override def previousOutput = empty.previousOutput
  override def scriptSignature = empty.scriptSignature
  override def sequence = empty.sequence
  override def scriptSigVarInt = empty.scriptSigVarInt
}
case class TransactionInputImpl(previousOutput : TransactionOutPoint, scriptSigVarInt : VarInt,
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput
