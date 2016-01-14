package org.scalacoin.marshallers.transaction

import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/14/16.
 */
class RawTransactionParserTest extends FlatSpec with MustMatchers {

  "RawTransactionParser" must "parse a raw transaction" in {
    val tx : Transaction = RawTransactionParser.read(TestUtil.rawTransaction)
    tx.version must be (1)
    tx.inputs.size must be (2)
    tx.outputs.size must be (2)
    tx.lockTime must be (0)
  }

/*  it must "write a raw transaction" in {
    val tx : Transaction = RawTransactionParser.read(TestUtil.rawTransaction)
    val serializedTx = RawTransactionParser.write(tx)

    serializedTx must be (TestUtil.rawTransaction)
  }*/
}
