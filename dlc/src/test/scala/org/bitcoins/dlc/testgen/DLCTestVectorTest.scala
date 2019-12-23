package org.bitcoins.dlc.testgen

import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

import scala.concurrent.Future

class DLCTestVectorTest extends BitcoinSAsyncTest {
  behavior of "DLCTestVector"

  val randomTestVectorGen: Gen[Future[DLCTestVector]] =
    Gen.const(()).map(_ => DLCTestVectorGenerator.generateRandomTestVector())

  it should "have serialization symmetry" in {
    forAllAsync(randomTestVectorGen) { testVectorF =>
      testVectorF.map { testVector =>
        assert(DLCTestVector.fromJson(testVector.toJson).contains(testVector))
      }
    }
  }
}
