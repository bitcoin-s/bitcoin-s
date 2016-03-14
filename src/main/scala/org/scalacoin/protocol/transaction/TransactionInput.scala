package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionInputParser, TransactionElement}
import org.scalacoin.protocol.{CompactSizeUInt}
import org.scalacoin.protocol.script.ScriptSignature

import org.scalacoin.util.{BitcoinSUtil, ScalacoinUtil}

/**
 * Created by chris on 12/26/15.
 */
sealed trait TransactionInput extends TransactionElement with TransactionInputFactory {

  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long


  def scriptSigCompactSizeUInt : CompactSizeUInt = BitcoinSUtil.parseCompactSizeUInt(scriptSignature)
  //https://bitcoin.org/en/developer-reference#txin
  override def size = previousOutput.size + scriptSignature.size +
    scriptSigCompactSizeUInt.size.toInt + 4

  def hex = RawTransactionInputParser.write(Seq(this))
}

object TransactionInput extends TransactionInput {
  override def previousOutput = empty.previousOutput
  override def scriptSignature = empty.scriptSignature
  override def sequence = empty.sequence
  override def scriptSigCompactSizeUInt = empty.scriptSigCompactSizeUInt
}

sealed case class TransactionInputImpl(previousOutput : TransactionOutPoint,
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput
