package org.scalacoin.protocol.transaction

import org.scalacoin.protocol.VarInt
import org.scalacoin.protocol.script.ScriptSignature

/**
 * Created by chris on 12/26/15.
 */
trait TransactionInput {
  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long
}

case class TransactionInputImpl(previousOutput : TransactionOutPoint,
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput
