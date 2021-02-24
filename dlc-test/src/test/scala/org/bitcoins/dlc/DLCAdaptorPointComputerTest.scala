package org.bitcoins.dlc

import org.bitcoins.core.protocol.tlv.OracleParamsV0TLV
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class DLCAdaptorPointComputerTest extends BitcoinSJvmTest with DLCTest {
  behavior of "DLCAdaptorPointComputer"

  it should "compute sigpoints correctly" in {
    runTestsForParam(Vector(4, 6, 8)) { numDigitsOrOutcomes =>
      runTestsForParam(Vector(true, false)) { isNumeric =>
        runTestsForParam(Vector((1, 1), (2, 3), (3, 5))) {
          case (threshold, numOracles) =>
            runTestsForParam(
              Vector(None,
                     Some(
                       OracleParamsV0TLV(numDigitsOrOutcomes / 2 + 1,
                                         numDigitsOrOutcomes / 2,
                                         maximizeCoverage = true)))) {
              oracleParams =>
                val contractParams = SingleContractParams(numDigitsOrOutcomes,
                                                          isNumeric,
                                                          threshold,
                                                          numOracles,
                                                          oracleParams)
                val (client, _, _) = constructDLCClients(contractParams)
                val contract = client.offer.contractInfo
                val outcomes = contract.allOutcomes

                val adaptorPoints = contract.adaptorPoints
                val expectedAdaptorPoints = outcomes.map(_.sigPoint)

                assert(adaptorPoints == expectedAdaptorPoints)
            }
        }
      }
    }
  }
}
