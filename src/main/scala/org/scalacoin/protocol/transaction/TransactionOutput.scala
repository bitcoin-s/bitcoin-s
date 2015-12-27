package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnit, Satoshis}
import org.scalacoin.protocol.VarInt

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutput {

  def value : CurrencyUnit
  def pkScriptLength : VarInt
  def pkScript : Seq[String]

}


case class TransactionOutputImpl(value : Satoshis, pkScriptLength : VarInt, pkScript : Seq[String]) extends TransactionOutput