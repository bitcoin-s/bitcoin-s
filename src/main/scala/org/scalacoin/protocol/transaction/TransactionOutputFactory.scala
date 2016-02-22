package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.scalacoin.protocol.script.{ScriptPubKey, ScriptPubKeyFactory}

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutputFactory { this : TransactionOutput =>


  def factory(newCurrencyUnit: CurrencyUnit) : TransactionOutput = TransactionOutputImpl(newCurrencyUnit,n,scriptPubKey)

  def factory(newOutputIndex : Int) : TransactionOutput = TransactionOutputImpl(value,newOutputIndex,scriptPubKey)

  def factory(newScriptPubKey : ScriptPubKey) : TransactionOutput = TransactionOutputImpl(value,n,newScriptPubKey)

  def factory(newCurrencyUnit: CurrencyUnit, newScriptPubKey: ScriptPubKey) : TransactionOutput = {
    TransactionOutputImpl(newCurrencyUnit,n,newScriptPubKey)
  }
  def empty : TransactionOutput = TransactionOutputImpl(CurrencyUnits.negativeSatoshi,0,ScriptPubKeyFactory.empty)

}
