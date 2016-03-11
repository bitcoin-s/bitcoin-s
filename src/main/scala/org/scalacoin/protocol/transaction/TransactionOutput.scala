package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.scalacoin.marshallers.transaction.{RawTransactionOutputParser, TransactionElement}
import org.scalacoin.protocol.CompactSizeUInt

import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.util.BitcoinSUtil


/**
 * Created by chris on 12/26/15.
 */
sealed trait TransactionOutput extends TransactionElement with TransactionOutputFactory {

  def value : CurrencyUnit
  def n : Int
  def scriptPubKey : ScriptPubKey
  def scriptPubKeyCompactSizeUInt : CompactSizeUInt = BitcoinSUtil.parseCompactSizeUInt(scriptPubKey)
  //https://bitcoin.org/en/developer-reference#txout
  override def size = scriptPubKey.size + scriptPubKeyCompactSizeUInt.size.toInt + 8

  override def hex = RawTransactionOutputParser.write(Seq(this))
}

object TransactionOutput extends TransactionOutput {
  def value = CurrencyUnits.negativeSatoshi
  def n = 0
  override def scriptPubKey = ScriptPubKey.empty

}


case class TransactionOutputImpl(value : CurrencyUnit, n : Int, scriptPubKey: ScriptPubKey) extends TransactionOutput