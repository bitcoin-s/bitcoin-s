package org.bitcoins.dlc.testgen

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.util.{Failure, Success}

class DLCTestVectorTest extends BitcoinSUnitTest {
  behavior of "DLCTestVectors"

  it should "have serialization symmetry" in {
    val gen = TestVectorUtil.testInputGen.map(DLCTxGen.successTestVector(_))

    forAll(gen) {
      case Success(testVector) =>
        assert(DLCTestVector.fromJson(testVector.toJson).contains(testVector))
      case Failure(err) => fail(err)
    }
  }

  it should "pass dlc_test" in {
    val vecResult = DLCTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(succeed) { case (_, testVec) =>
      testVec match {
        case testVec: SuccessTestVector =>
          val resultT = DLCTxGen
            .successTestVector(testVec.testInputs)

          resultT match {
            case Success(regenerated) => assert(regenerated == testVec)
            case Failure(err)         => fail(err)
          }
      }
    }
  }
}
