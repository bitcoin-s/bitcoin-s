package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.TestUtil
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutPointTest extends FlatSpec with MustMatchers {
  "TransactionOutPoint" must "define an empty transaction outpoint" in {
    EmptyTransactionOutPoint.txId.hex must be("0000000000000000000000000000000000000000000000000000000000000000")
    EmptyTransactionOutPoint.vout must be(UInt32.max)
  }

  it must "read then write a transaction outpoint" in {

    val outPoint = TestUtil.simpleTransaction.inputs.head.previousOutput
    TransactionOutPoint(outPoint.hex).hex must be(outPoint.hex)
  }
}
