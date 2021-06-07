package org.bitcoins.dlc.statictest

import org.bitcoins.core.util.FutureUtil
import org.bitcoins.dlc.testgen._
import org.bitcoins.testkitcore.gen.FeeUnitGen
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import org.scalacheck.Gen
import org.scalatest.Assertion
import play.api.libs.json.JsResult

import scala.concurrent.Future

class DLCFeeTestVectorTest extends BitcoinSJvmTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "DLCFeeTestVector"

  it should "have serialization symmetry" in {
    val inputGen = for {
      redeemScriptLen <- Gen.oneOf(0, 22, 34)
      maxWitnessLen <- Gen.oneOf(Gen.choose(1, 300), Gen.oneOf(107, 108))
    } yield FundingFeeInfo(redeemScriptLen, maxWitnessLen)

    val gen = for {
      numOfferInputs <- Gen.choose(1, 10)
      offerInputs <- Gen.listOfN(numOfferInputs, inputGen)
      offerPayoutSPKLen <- Gen.oneOf(22, 25, 34, 35, 71, 173)
      offerChangeSPKLen <- Gen.oneOf(22, 34)
      numAcceptInputs <- Gen.choose(1, 10)
      acceptInputs <- Gen.listOfN(numAcceptInputs, inputGen)
      acceptPayoutSPKLen <- Gen.oneOf(22, 25, 34, 35, 71, 173)
      acceptChangeSPKLen <- Gen.oneOf(22, 34)
      feeRate <- FeeUnitGen.satsPerVirtualByte
    } yield {
      DLCFeeTestVector(offerInputs.toVector,
                       offerPayoutSPKLen,
                       offerChangeSPKLen,
                       acceptInputs.toVector,
                       acceptPayoutSPKLen,
                       acceptChangeSPKLen,
                       feeRate)
    }

    forAll(gen) { feeTest =>
      val feeTestResult = DLCFeeTestVector.fromJson(feeTest.toJson)
      assert(feeTestResult.isSuccess)
      assert(feeTestResult.get == feeTest)
    }
  }

  it should "pass dlc_fee_test" in {
    val vecResult: JsResult[Vector[DLCFeeTestVector]] =
      DLCFeeTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    def assertBatch(vec: Vector[DLCFeeTestVector]): Future[Vector[Assertion]] =
      Future {
        vec.map { case testVec =>
          assert(DLCFeeTestVector.apply(testVec.inputs) == testVec)
        }
      }

    val assertionsF = FutureUtil
      .batchAndParallelExecute(vecResult.get, assertBatch)
      .map(_.flatten)

    assertionsF.map(_ => succeed)
  }
}
