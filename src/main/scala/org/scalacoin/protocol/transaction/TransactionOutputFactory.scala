package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.scalacoin.marshallers.transaction.RawTransactionOutputParser
import org.scalacoin.protocol.script.{ScriptPubKey, ScriptPubKeyFactory}
import org.scalacoin.util.Factory

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutputFactory extends Factory[TransactionOutput] { this : TransactionOutput =>


  def factory(newCurrencyUnit: CurrencyUnit) : TransactionOutput = TransactionOutputImpl(newCurrencyUnit,n,scriptPubKey)

  def factory(newOutputIndex : Int) : TransactionOutput = TransactionOutputImpl(value,newOutputIndex,scriptPubKey)

  def factory(newScriptPubKey : ScriptPubKey) : TransactionOutput = TransactionOutputImpl(value,n,newScriptPubKey)

  def factory(newCurrencyUnit: CurrencyUnit, newScriptPubKey: ScriptPubKey) : TransactionOutput = {
    TransactionOutputImpl(newCurrencyUnit,n,newScriptPubKey)
  }
  def empty : TransactionOutput = TransactionOutputImpl(CurrencyUnits.negativeSatoshi,0,ScriptPubKeyFactory.empty)

  //TODO: This could bomb if the transaction output is not in the right format,
  //probably should put more thought into this to make it more robust
  def fromBytes(bytes : Seq[Byte]) : TransactionOutput = RawTransactionOutputParser.read(bytes).head

}
