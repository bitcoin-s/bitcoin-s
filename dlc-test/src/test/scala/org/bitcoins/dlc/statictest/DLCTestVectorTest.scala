package org.bitcoins.dlc.statictest

import org.bitcoins.dlc.testgen._
import org.bitcoins.testkitcore.util.{BitcoinSJvmTest}
import org.scalatest.Assertion

import scala.concurrent.Future
import scala.util.{Failure, Success}

class DLCTestVectorTest extends BitcoinSJvmTest {
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

    val vecF = vecResult.get.map(runTest(_))
    Future
      .sequence(vecF)
      .map(_ => succeed)
  }

  private def runTest(testVec: TestVector): Future[Assertion] = {
    Future {
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
