package org.scalacoin.protocol.transaction

import org.scalacoin.currency.CurrencyUnits
import org.scalacoin.protocol.script.EmptyScriptPubKey
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutputTest extends FlatSpec with MustMatchers  {

  "TransactionOutput" must "define an empty transaction output" in {
    EmptyTransactionOutput.scriptPubKey must be (EmptyScriptPubKey)
    EmptyTransactionOutput.value must be (CurrencyUnits.negativeSatoshi)
  }
}
