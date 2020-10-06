package org.bitcoins.dlc.testgen

import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

import scala.concurrent.Future

class DLCTxTestVectorTest extends BitcoinSAsyncTest {
  behavior of "DLCTxTestVector"

  it should "have serialization symmetry" in {
    val gen = Gen.choose(2, 100).map(DLCTxGen.randomTxTestVector)

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
