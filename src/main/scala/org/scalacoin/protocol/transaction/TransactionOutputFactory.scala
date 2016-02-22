package org.scalacoin.protocol.transaction

import org.scalacoin.currency.{CurrencyUnit, Satoshis}
import org.scalacoin.protocol.script.{ScriptPubKey, ScriptPubKeyFactory}

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutputFactory { this : TransactionOutput =>


  def factory(currencyUnit: CurrencyUnit) : TransactionOutput = TransactionOutputImpl(currencyUnit,n,scriptPubKey)

  def factory(newOutputIndex : Int) : TransactionOutput = TransactionOutputImpl(value,newOutputIndex,scriptPubKey)

  def factory(newScriptPubKey : ScriptPubKey) : TransactionOutput = TransactionOutputImpl(value,n,newScriptPubKey)

  def empty : TransactionOutput = TransactionOutputImpl(Satoshis(0),0,ScriptPubKeyFactory.empty)

}
