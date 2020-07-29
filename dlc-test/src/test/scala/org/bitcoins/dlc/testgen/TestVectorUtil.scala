package org.bitcoins.dlc.testgen

import org.scalacheck.Gen

object TestVectorUtil {

  val p2wpkhInputGen: Gen[FundingInputTx] =
    Gen.const(()).map(_ => DLCTxGen.fundingInputTx())

  val p2wshInputGen: Gen[FundingInputTx] =
    Gen.const(()).map(_ => DLCTxGen.multiSigFundingInputTx())

  val p2shInputGen: Gen[FundingInputTx] =
    Gen.const(()).map(_ => DLCTxGen.multiSigFundingInputTx(p2shNested = true))

  val inputGen: Gen[FundingInputTx] =
    Gen.oneOf(p2wpkhInputGen, p2wshInputGen, p2shInputGen)

  val inputsGen: Gen[List[FundingInputTx]] =
    Gen.oneOf(1, 2, 3, 8).flatMap(Gen.listOfN(_, inputGen))

  val testInputGen: Gen[ValidTestInputs] =
    Gen.choose(2, 100).flatMap { numOutcomes =>
      inputsGen.flatMap { offerInputs =>
        inputsGen.flatMap { acceptInputs =>
          DLCTxGen.validTestInputsForInputs(offerInputs.toVector,
                                            acceptInputs.toVector,
                                            numOutcomes)
        }
      }
    }
}
