package org.bitcoins.protocol.transaction

import org.bitcoins.protocol.script.EmptyScriptSignature
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionInputTest extends FlatSpec with MustMatchers {


  "TransactionInput" must "define an empty transaction input" in {
    EmptyTransactionInput.previousOutput must be (EmptyTransactionOutPoint)
    EmptyTransactionInput.scriptSignature must be (EmptyScriptSignature)
    EmptyTransactionInput.scriptSigCompactSizeUInt.num must be (0)
    EmptyTransactionInput.scriptSigCompactSizeUInt.size must be (1)
    EmptyTransactionInput.sequence must be (TransactionConstants.sequence)
  }


  it must "write a transaction output to hex accurately" in {
    TransactionInput.fromHex(TestUtil.rawTxInput).hex must be (TestUtil.rawTxInput)
  }
}
