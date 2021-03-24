package org.bitcoins.dlc.testgen

import org.bitcoins.testkitcore.util.BitcoinSJvmTest

import scala.concurrent.Future

class DLCTestVectorTest extends BitcoinSJvmTest {
  behavior of "DLCTestVectors"

  it should "have serialization symmetry" in {
    val gen = TestVectorUtil.testInputGen.map(DLCTxGen.successTestVector(_))

    forAllAsync(gen) { testVectorF =>
      testVectorF.map { testVector =>
        assert(DLCTestVector.fromJson(testVector.toJson).contains(testVector))
      }
    }
  }

  it should "pass dlc_test" in {
    val vecResult = DLCTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(Future.successful(succeed)) {
      case (assertF, testVec) =>
        assertF.flatMap { _ =>
          testVec match {
            case testVec: SuccessTestVector =>
              DLCTxGen
                .successTestVector(testVec.testInputs)
                .map(regenerated => assert(regenerated == testVec))
          }
        }
    }
  }
}
