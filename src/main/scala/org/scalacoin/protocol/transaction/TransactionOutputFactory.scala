package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.scalacoin.marshallers.transaction.RawTransactionOutputParser
import org.scalacoin.protocol.script.{ScriptPubKey, ScriptPubKeyFactory}
import org.scalacoin.util.Factory

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutputFactory extends Factory[TransactionOutput] {


  def factory(oldOutput : TransactionOutput, newCurrencyUnit: CurrencyUnit) : TransactionOutput =
    TransactionOutputImpl(newCurrencyUnit,oldOutput.scriptPubKey)

  def factory(oldOutput : TransactionOutput, newScriptPubKey : ScriptPubKey) : TransactionOutput =
    TransactionOutputImpl(oldOutput.value,newScriptPubKey)

  def factory(oldOutput : TransactionOutput, newCurrencyUnit: CurrencyUnit, newScriptPubKey: ScriptPubKey) : TransactionOutput = {
    TransactionOutputImpl(newCurrencyUnit,newScriptPubKey)
  }

  def factory(currencyUnit: CurrencyUnit, scriptPubKey: ScriptPubKey) = TransactionOutputImpl(currencyUnit,scriptPubKey)
  def empty : TransactionOutput = TransactionOutputImpl(CurrencyUnits.negativeSatoshi,ScriptPubKeyFactory.empty)

  //TODO: This could bomb if the transaction output is not in the right format,
  //probably should put more thought into this to make it more robust
  def fromBytes(bytes : Seq[Byte]) : TransactionOutput = RawTransactionOutputParser.read(bytes).head

}


object TransactionOutputFactory extends TransactionOutputFactory