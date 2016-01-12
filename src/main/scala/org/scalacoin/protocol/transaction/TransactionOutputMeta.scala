package org.scalacoin.protocol.transaction

import org.scalacoin.currency.CurrencyUnit
import org.scalacoin.protocol.script.ScriptPubKey

/**
 * Created by Tom on 1/12/2016.
 */
trait TransactionOutputMeta {
  def bestBlock : String
  def confirmations : Int
  def version : Int
  def coinbase : Boolean
  def value : CurrencyUnit
  def scriptPubKey : ScriptPubKey
}

case class TransactionOutputMetaImpl(bestBlock : String, confirmations: Int, version: Int,
   coinbase : Boolean, value : CurrencyUnit, scriptPubKey: ScriptPubKey) extends TransactionOutputMeta
