package org.bitcoins.dlc.testgen

import org.bitcoins.testkitcore.gen.FeeUnitGen
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen

class DLCFeeTestVectorTest extends BitcoinSUnitTest {

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
    val vecResult = DLCFeeTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(succeed) { case (_, testVec) =>
      assert(DLCFeeTestVector(testVec.inputs) == testVec)
    }
  }
}
