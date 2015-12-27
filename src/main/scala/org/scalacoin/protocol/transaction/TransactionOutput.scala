package org.scalacoin.protocol.transaction

import org.scalacoin.currency.Satoshis
import org.scalacoin.protocol.VarInt

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutput {

  def value : Satoshis
  def pkScriptLength : VarInt
  def pkScript : Seq[Char]

}


case class TransactionOutputImpl(value : Satoshis, pkScriptLength : VarInt, pkScript : Seq[Char]) extends TransactionOutput