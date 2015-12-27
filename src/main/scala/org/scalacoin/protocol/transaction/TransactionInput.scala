package org.scalacoin.protocol.transaction

import org.scalacoin.protocol.VarInt

/**
 * Created by chris on 12/26/15.
 */
trait TransactionInput {
  def previousOutput : OutPoint
  def scriptLength : VarInt
  def scriptSignature : Seq[Char]
  def sequence : Long
}
