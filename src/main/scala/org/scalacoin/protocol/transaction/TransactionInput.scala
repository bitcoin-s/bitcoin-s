package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionInputParser, TransactionElement}
import org.scalacoin.protocol.{CompactSizeUInt}
import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 12/26/15.
 */
trait TransactionInput extends TransactionElement {
  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long

  def scriptSigCompactSizeUInt : CompactSizeUInt = ScalacoinUtil.parseCompactSizeUInt(scriptSignature)
  //https://bitcoin.org/en/developer-reference#txin
  override def size = previousOutput.size + scriptSignature.size +
    scriptSigCompactSizeUInt.size.toInt + 4

  def hex = RawTransactionInputParser.write(Seq(this))
}

case class TransactionInputImpl(previousOutput : TransactionOutPoint, /*override val scriptSigCompactSizeUInt : CompactSizeUInt,*/
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput
