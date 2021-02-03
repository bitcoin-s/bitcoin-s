package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.testkit.util.BitcoinSUnitTest

/** Created by chris on 3/30/16.
  */
class TransactionOutputTest extends BitcoinSUnitTest {

  "TransactionOutput" must "define an empty transaction output" in {
    EmptyTransactionOutput.scriptPubKey must be(EmptyScriptPubKey)
    EmptyTransactionOutput.value must be(CurrencyUnits.negativeSatoshi)
  }
}
