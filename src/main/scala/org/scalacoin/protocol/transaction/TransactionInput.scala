package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.protocol.VarInt
import org.scalacoin.protocol.script.ScriptSignature

/**
 * Created by chris on 12/26/15.
 */
trait TransactionInput extends TransactionElement {
  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long

  //https://bitcoin.org/en/developer-reference#txin
  def size = previousOutput.size + scriptSignature.size + 4
}

case class TransactionInputImpl(previousOutput : TransactionOutPoint,
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput
