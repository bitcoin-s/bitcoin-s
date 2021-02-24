package org.bitcoins.dlc

import org.bitcoins.core.protocol.tlv.OracleParamsV0TLV
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class DisjointUnionDLCTest extends BitcoinSJvmTest with DLCTest {
  behavior of "Disjoint Union DLC"

  it should "be able to construct and verify with ScriptInterpreter every tx in a double enum contract" in {
    val numDisjoint = 2
    val numOutcomes = 10
    val singleParams = 0.until(numDisjoint).toVector.map { _ =>
      EnumContractParams(numOutcomes, oracleThreshold = 1, numOracles = 1)
    }
    val contractParams = DisjointUnionContractParams(singleParams)
    val outcomes = 0.until(numDisjoint).toVector.flatMap { contractIndex =>
      0L.until(numOutcomes).toVector.map { outcomeIndex =>
        (contractIndex, outcomeIndex)
      }
    }

    executeForCasesInUnion(outcomes, contractParams)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a double numeric contract" in {
    val numDisjoint = 2
    val numDigits = 10
    val singleParams = 0.until(numDisjoint).toVector.map { _ =>
      val oracleParams = OracleParamsV0TLV(maxErrorExp = 6,
                                           minFailExp = 2,
                                           maximizeCoverage = true)
      NumericContractParams(numDigits,
                            oracleThreshold = 1,
                            numOracles = 1,
                            Some(oracleParams))
    }
    val contractParams = DisjointUnionContractParams(singleParams)
    val outcomes = 0.until(numDisjoint).toVector.flatMap { contractIndex =>
      tenRandomNums(numDigits).map { outcomeIndex =>
        (contractIndex, outcomeIndex)
      }
    }

    executeForCasesInUnion(outcomes, contractParams)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a mixed enum and numeric contract" in {
    val numOutcomes = 10
    val numDigits = 10
    val enumParams =
      EnumContractParams(numOutcomes, oracleThreshold = 1, numOracles = 1)
    val oracleParams = OracleParamsV0TLV(maxErrorExp = 6,
                                         minFailExp = 2,
                                         maximizeCoverage = true)
    val numericParams =
      NumericContractParams(numDigits,
                            oracleThreshold = 1,
                            numOracles = 1,
                            Some(oracleParams))

    val contractParams =
      DisjointUnionContractParams(Vector(enumParams, numericParams))

    val enumOutcomes = 0L.until(numOutcomes).toVector.map((0, _))
    val numericOutcomes = tenRandomNums(numDigits).map((1, _))
    val outcomes = enumOutcomes ++ numericOutcomes

    executeForCasesInUnion(outcomes, contractParams)
  }
}
