package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.scalacoin.marshallers.transaction.{RawTransactionOutputParser, TransactionElement}
import org.scalacoin.protocol.CompactSizeUInt

import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.util.BitcoinSUtil


/**
 * Created by chris on 12/26/15.
 */
sealed trait TransactionOutput extends TransactionElement {

  def value : CurrencyUnit
  def scriptPubKey : ScriptPubKey
  def scriptPubKeyCompactSizeUInt : CompactSizeUInt = BitcoinSUtil.parseCompactSizeUInt(scriptPubKey)
  //https://bitcoin.org/en/developer-reference#txout
  override def size = scriptPubKey.size + scriptPubKeyCompactSizeUInt.size.toInt + 8

  override def hex = RawTransactionOutputParser.write(Seq(this))
}

case object TransactionOutput extends TransactionOutput {
  override def value = CurrencyUnits.negativeSatoshi
  override def scriptPubKey = ScriptPubKeyFactory.empty
}


case class TransactionOutputImpl(value : CurrencyUnit, scriptPubKey: ScriptPubKey) extends TransactionOutput