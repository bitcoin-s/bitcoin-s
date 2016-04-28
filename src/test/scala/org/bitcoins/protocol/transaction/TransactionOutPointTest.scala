package org.bitcoins.protocol.transaction

import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutPointTest extends FlatSpec with MustMatchers {
  "TransactionOutPoint" must "define an empty transaction outpoint" in {
    EmptyTransactionOutPoint.txId must be ("")
    EmptyTransactionOutPoint.vout must be (-1)
  }

  it must "read then write a transaction outpoint" in {

    val outPoint = TestUtil.simpleTransaction.inputs.head.previousOutput
    TransactionOutPoint(outPoint.hex).hex must be (outPoint.hex)
  }
}
