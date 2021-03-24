package org.bitcoins.dlc.testgen

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DLCTxTestVectorTest extends BitcoinSUnitTest {
  behavior of "DLCTxTestVector"

  it should "have serialization symmetry" in {
    val gen = TestVectorUtil.testInputGen.map(DLCTxGen.dlcTxTestVector)

    forAll(gen) { testVec =>
      val testVecResult = DLCTxTestVector.fromJson(testVec.toJson)
      assert(testVecResult.isSuccess)
      assert(testVecResult.get == testVec)
    }
  }

  it should "pass dlc_tx_test" in {
    val vecResult = DLCTxTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(succeed) { case (_, testVec) =>
      val regenerated = DLCTxGen.dlcTxTestVector(testVec.inputs)
      assert(regenerated == testVec)
    }
  }
}
