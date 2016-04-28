package org.bitcoins.protocol.transaction

import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutPointFactoryTest extends FlatSpec with MustMatchers  {

  "TransactionOutPointFactory" must "create an outpoint from its base components" in {
    val outPoint = TransactionOutPoint(TestUtil.parentSimpleTransaction.outputs(0), TestUtil.parentSimpleTransaction)
    outPoint.vout must be (0)
    outPoint.txId must be (TestUtil.parentSimpleTransaction.txId)
  }

  it must "throw an exception if the given output is not part of the given transaciton" in {
    intercept[RuntimeException] {
      val outPoint = TransactionOutPoint(TestUtil.simpleTransaction.outputs(0), TestUtil.parentSimpleTransaction)
    }
  }
}
