package org.bitcoins.dlc

import org.bitcoins.dlc.testgen.{
  DLCTestVector,
  DLCTestVectorGen,
  DLCTxGen,
  SuccessTestVector
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

import scala.concurrent.Future

class DLCTestVectorTest extends BitcoinSAsyncTest {
  behavior of "DLCTestVectors"

  val randomTestVectorGen: Gen[Future[DLCTestVector]] =
    Gen.choose(2, 100).map(num => DLCTxGen.randomSuccessTestVector(num))

  it should "have serialization symmetry" in {
    forAllAsync(randomTestVectorGen) { testVectorF =>
      testVectorF.map { testVector =>
        assert(DLCTestVector.fromJson(testVector.toJson).contains(testVector))
      }
    }
  }

  it should "pass dlc_tests" in {
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
