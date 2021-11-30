package org.bitcoins.dlc

import org.bitcoins.core.protocol.tlv.{
  OracleParamsV0TLV,
  UnsignedNumericOutcome
}
import org.bitcoins.core.util.{EnvUtil, NumberUtil}
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

import scala.util.Random

class NumericDLCTest extends BitcoinSJvmTest with DLCTest {
  behavior of "Numeric DLC"

  val numericOracleSchemesToTest: Vector[(Int, Int)] =
    Vector((1, 1), (2, 2), (2, 3))
  val numDigitsToTest: Vector[Int] = Vector(4, 5, 10)

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal numeric case" in {
    runTestsForParam(numDigitsToTest) { numDigits =>
      runTestsForParam(numericOracleSchemesToTest) {
        case (threshold, numOracles) =>
          val contractParams =
            NumericContractParams(numDigits, threshold, numOracles)
          val nums = tenRandomNums(numDigits)

          executeForCases(nums, contractParams)
      }
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the large numeric case" in {
    val numDigits = if (EnvUtil.isNativeSecp256k1Disabled) {
      12
    } else {
      //optimization for CI, tests are much slower when secp256k1 isnt used
      17
    }
    val contractParams =
      NumericContractParams(numDigits, oracleThreshold = 1, numOracles = 1)
    val nums = tenRandomNums(numDigits)

    executeForCases(nums, contractParams)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for a normal multi-oracle numeric case with bounded differences allowed" in {
    val threshold = 3
    val numOracles = 5
    val numDigits = 8
    val params = OracleParamsV0TLV(maxErrorExp = 4,
                                   minFailExp = 2,
                                   maximizeCoverage = false)
    val contractParams =
      NumericContractParams(numDigits, threshold, numOracles, Some(params))
    val nums = tenRandomNums(numDigits)

    executeForCases(nums, contractParams)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund numeric case" in {
    runTestsForParam(numDigitsToTest) { numDigits =>
      runTestsForParam(numericOracleSchemesToTest) {
        case (threshold, numOracles) =>
          val contractParams =
            NumericContractParams(numDigits, threshold, numOracles)

          executeRefundCase(contractParams)
      }
    }
  }

  it should "be able to derive aggregate oracle signature from remote CET signatures" in {
    // Larger numbers of digits make tests take too long.
    val numDigitsToTest = Vector(5, 9)
    runTestsForParam(numDigitsToTest) { numDigits =>
      runTestsForParam(Vector((1, 1), (2, 2), (2, 3))) {
        case (threshold, numOracles) =>
          val oracleParamOptsToTest = if (threshold > 1) {
            Vector(None,
                   Some(
                     OracleParamsV0TLV(numDigits - 2,
                                       numDigits - 4,
                                       maximizeCoverage = false)))
          } else Vector(None)
          runTestsForParam(oracleParamOptsToTest) { oracleParamsOpt =>
            val max = (1L << numDigits) - 1
            val outcomesToTest = 0
              .until(9)
              .toVector
              .map(num => (max / num.toDouble).toLong)
              .map(num => NumberUtil.decompose(num, 2, numDigits))
            val contractParams = NumericContractParams(numDigits,
                                                       threshold,
                                                       numOracles,
                                                       oracleParamsOpt)

            constructAndSetupDLC(contractParams).flatMap {
              case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
                runTestsForParam(outcomesToTest) { outcomeToTest =>
                  val possibleOutcomes = outcomes
                    .collect { case ds: UnsignedNumericOutcome => ds }
                    .filter(outcome => outcomeToTest.startsWith(outcome.digits))
                  val outcome =
                    possibleOutcomes(Random.nextInt(possibleOutcomes.length))

                  val oracleInfo = dlcOffer.offer.oracleInfos.head

                  val oracleIndices =
                    0.until(oracleInfo.numOracles).toVector
                  val chosenOracles =
                    Random
                      .shuffle(oracleIndices)
                      .take(oracleInfo.threshold)
                      .sorted

                  val oracleOutcome =
                    genNumericOracleOutcome(chosenOracles,
                                            dlcOffer.offer.contractInfo,
                                            outcome.digits,
                                            oracleParamsOpt)

                  val oracleSigs = genNumericOracleSignatures(oracleOutcome)

                  assertCorrectSigDerivation(offerSetup = offerSetup,
                                             dlcOffer = dlcOffer,
                                             acceptSetup = acceptSetup,
                                             dlcAccept = dlcAccept,
                                             oracleSigs = oracleSigs,
                                             outcome = oracleOutcome)
                }
            }
          }
      }
    }
  }
}
