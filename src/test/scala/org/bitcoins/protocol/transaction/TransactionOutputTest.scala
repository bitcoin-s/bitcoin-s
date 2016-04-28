package org.bitcoins.protocol.transaction

import org.bitcoins.currency.CurrencyUnits
import org.bitcoins.protocol.script.EmptyScriptPubKey
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutputTest extends FlatSpec with MustMatchers  {

  "TransactionOutput" must "define an empty transaction output" in {
    EmptyTransactionOutput.scriptPubKey must be(EmptyScriptPubKey)
    EmptyTransactionOutput.value must be(CurrencyUnits.negativeSatoshi)
  }
}
