package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnit, Satoshis}
import org.scalacoin.protocol.VarInt
import org.scalacoin.protocol.script.ScriptPubKey

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutput {

  def value : CurrencyUnit
  def n : Int
  def scriptPubKey : ScriptPubKey

}


case class TransactionOutputImpl(value : CurrencyUnit, n : Int, scriptPubKey: ScriptPubKey) extends TransactionOutput