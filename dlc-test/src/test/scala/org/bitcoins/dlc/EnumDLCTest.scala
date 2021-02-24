package org.bitcoins.dlc

import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class EnumDLCTest extends BitcoinSJvmTest with DLCTest {
  behavior of "Enum DLC"

  val enumOracleSchemesToTest: Vector[(Int, Int)] =
    Vector((1, 1), (1, 2), (2, 2), (2, 3), (3, 5), (5, 8))

  val numEnumOutcomesToTest: Vector[Int] = 2.until(10).toVector

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal enum case" in {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(enumOracleSchemesToTest) {
        case (threshold, numOracles) =>
          val contractParams =
            EnumContractParams(numOutcomes, threshold, numOracles)
          val outcomes = 0L.until(numOutcomes).toVector

          executeForCases(outcomes, contractParams)
      }
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund enum case" in {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(enumOracleSchemesToTest) {
        case (threshold, numOracles) =>
          val contractParams =
            EnumContractParams(numOutcomes, threshold, numOracles)

          executeRefundCase(contractParams)
      }
    }
  }

  it should "all work for a 100 outcome DLC" in {
    val numOutcomes = 100
    val outcomes = 0L.until(numOutcomes).toVector
    val contractParams =
      EnumContractParams(numOutcomes, oracleThreshold = 1, numOracles = 1)

    for {
      _ <- executeForCases(outcomes, contractParams)
      _ <- executeRefundCase(contractParams)
    } yield succeed
  }

  it should "be able to derive oracle signature from remote CET signature" in {
    val outcomeIndex = 1

    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(enumOracleSchemesToTest) {
        case (threshold, numOracles) =>
          val contractParams =
            EnumContractParams(numOutcomes, threshold, numOracles)

          constructAndSetupDLC(contractParams).flatMap {
            case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
              val (oracleOutcome, sigs) =
                genOracleOutcomeAndSignatures(
                  numOutcomes,
                  isNumeric = false,
                  dlcOffer.offer.contractInfo.contracts.head,
                  outcomes,
                  outcomeIndex,
                  paramsOpt = None)

              assertCorrectSigDerivation(offerSetup = offerSetup,
                                         dlcOffer = dlcOffer,
                                         acceptSetup = acceptSetup,
                                         dlcAccept = dlcAccept,
                                         oracleSigs = sigs,
                                         outcome = oracleOutcome)
          }
      }
    }
  }
}
