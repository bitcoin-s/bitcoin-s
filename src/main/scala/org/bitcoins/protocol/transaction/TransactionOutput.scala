package org.bitcoins.protocol.transaction

import org.bitcoins.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.protocol.VarInt
import org.bitcoins.protocol.script.ScriptPubKey

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutput {

  def value : CurrencyUnit
  def n : Int
  def scriptPubKey : ScriptPubKey

}


case class TransactionOutputImpl(value : CurrencyUnit, n : Int, scriptPubKey: ScriptPubKey) extends TransactionOutput