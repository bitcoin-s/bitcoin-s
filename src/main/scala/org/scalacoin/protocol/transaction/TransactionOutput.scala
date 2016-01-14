package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnit, Satoshis}
import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.protocol.VarInt
import org.scalacoin.protocol.script.ScriptPubKey

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutput extends TransactionElement {

  def value : CurrencyUnit
  def n : Int
  def scriptPubKey : ScriptPubKey

  //https://bitcoin.org/en/developer-reference#txout
  override def size = scriptPubKey.size + 8

}


case class TransactionOutputImpl(value : CurrencyUnit, n : Int, scriptPubKey: ScriptPubKey) extends TransactionOutput