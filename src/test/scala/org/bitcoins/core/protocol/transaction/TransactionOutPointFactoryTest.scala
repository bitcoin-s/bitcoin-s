package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutPointFactoryTest extends FlatSpec with MustMatchers  {

  "TransactionOutPointFactory" must "create an outpoint from its base components" in {
    val outPoint = TransactionOutPoint(TestUtil.parentSimpleTransaction.outputs.head, TestUtil.parentSimpleTransaction)
    outPoint.vout must be (UInt32.zero)
    outPoint.txId must be (TestUtil.parentSimpleTransaction.txId)
  }

  it must "throw an exception if the given output is not part of the given transaciton" in {
    intercept[RuntimeException] {
      TransactionOutPoint(TestUtil.simpleTransaction.outputs.head, TestUtil.parentSimpleTransaction)
    }
  }
}
