package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.scalacoin.marshallers.transaction.{RawTransactionOutputParser, TransactionElement}

import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}



/**
 * Created by chris on 12/26/15.
 */
sealed trait TransactionOutput extends TransactionElement with TransactionOutputFactory {

  def value : CurrencyUnit
  def n : Int
  def scriptPubKey : ScriptPubKey

  //https://bitcoin.org/en/developer-reference#txout
  override def size = scriptPubKey.size + 8

  override def hex = RawTransactionOutputParser.write(Seq(this))
}

object TransactionOutput extends TransactionOutput {
  def value = CurrencyUnits.negativeSatoshi
  def n = 0
  override def scriptPubKey = ScriptPubKeyFactory.empty

}


case class TransactionOutputImpl(value : CurrencyUnit, n : Int, scriptPubKey: ScriptPubKey) extends TransactionOutput