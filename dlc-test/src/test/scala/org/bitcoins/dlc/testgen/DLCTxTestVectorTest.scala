package org.bitcoins.dlc.testgen

import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future

class DLCTxTestVectorTest extends BitcoinSAsyncTest {
  behavior of "DLCTxTestVector"

  it should "have serialization symmetry" in {
    val gen = TestVectorUtil.testInputGen.map(DLCTxGen.dlcTxTestVector(_))

    forAllAsync(gen) { testVecF =>
      testVecF.map { testVec =>
        val testVecResult = DLCTxTestVector.fromJson(testVec.toJson)
        assert(testVecResult.isSuccess)
        assert(testVecResult.get == testVec)
      }
    }
  }

  it should "pass dlc_tx_test" in {
    val vecResult = DLCTxTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(Future.successful(succeed)) {
      case (assertF, testVec) =>
        assertF.flatMap { _ =>
          DLCTxGen
            .dlcTxTestVector(testVec.inputs)
            .map(regenerated => assert(regenerated == testVec))
        }
    }
  }
}
