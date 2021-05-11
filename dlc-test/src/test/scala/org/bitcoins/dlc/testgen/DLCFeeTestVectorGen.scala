package org.bitcoins.dlc.testgen

import java.io.File

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object DLCFeeTestVectorGen
    extends TestVectorGen[DLCFeeTestVector, DLCFeeTestVectorInput] {

  override val defaultTestFile: File = new File(
    "dlc-test/src/test/scala/org/bitcoins/dlc/testgen/dlc_fee_test.json")

  override val testVectorParser: DLCFeeTestVector.type =
    DLCFeeTestVector

  override def inputFromJson: JsValue => JsResult[DLCFeeTestVectorInput] =
    DLCFeeTestVectorInput.fromJson

  override val inputStr: String = "inputs"

  override def generateFromInput: DLCFeeTestVectorInput => Future[
    DLCFeeTestVector] = { input =>
    Future.successful(DLCFeeTestVector(input))
  }

  override def generateTestVectors(): Future[Vector[DLCFeeTestVector]] = {
    val redeemScriptLens = Vector(0, 22, 34)
    val maxWitnessLens = Vector(108, 133, 218)
    val feeFundingInfo1 = FundingFeeInfo(0, 108)
    val feeFundingInfo2 = FundingFeeInfo(22, 108)
    val feeFundingInfo3 = FundingFeeInfo(34, 218)
    val oneInput = Vector(feeFundingInfo1)
    val twoInputs = Vector(feeFundingInfo2, feeFundingInfo3)
    val feeFundingInfos = redeemScriptLens.flatMap { redeemScriptLen =>
      maxWitnessLens.flatMap { maxWitnessLen =>
        if (
          redeemScriptLen == 22 && (maxWitnessLen != 107 || maxWitnessLen != 108)
        ) {
          None
        } else {
          Some(FundingFeeInfo(redeemScriptLen, maxWitnessLen))
        }
      }
    }

    val payoutSPKLens = Vector(22, 25, 34, 35, 71, 173)
    val changeSPKLens = Vector(22, 34)
    val feeRates = Vector(1L, 5L, 10L)
      .map(Satoshis.apply)
      .map(SatoshisPerVirtualByte.apply)

    def allTests(
        offerInputs: Vector[FundingFeeInfo],
        acceptInputs: Vector[FundingFeeInfo]): Vector[DLCFeeTestVector] = {
      for {
        offerPayoutSPKLen <- payoutSPKLens
        offerChangeSPKLen <- changeSPKLens
        acceptPayoutSPKLen <- payoutSPKLens
        acceptChangeSPKLen <- changeSPKLens
        feeRate <- feeRates
      } yield {
        DLCFeeTestVector(
          offerInputs,
          offerPayoutSPKLen,
          offerChangeSPKLen,
          acceptInputs,
          acceptPayoutSPKLen,
          acceptChangeSPKLen,
          feeRate
        )
      }
    }

    def someTests(
        offerInputs: Vector[FundingFeeInfo],
        acceptInputs: Vector[FundingFeeInfo]): Vector[DLCFeeTestVector] = {
      allTests(offerInputs, acceptInputs)
        .sortBy(_ => scala.util.Random.nextDouble())
        .take(10)
    }

    val tests = allTests(oneInput, oneInput) ++
      someTests(twoInputs, twoInputs) ++
      someTests(oneInput, twoInputs) ++
      someTests(twoInputs, oneInput) ++
      someTests(feeFundingInfos, feeFundingInfos)

    Future.successful(tests)
  }
}
