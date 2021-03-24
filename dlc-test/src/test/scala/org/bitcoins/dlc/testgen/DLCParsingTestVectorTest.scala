package org.bitcoins.dlc.testgen

import org.bitcoins.core.protocol.tlv.{LnMessage, TLV}
import org.bitcoins.crypto.NetworkElement
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen

class DLCParsingTestVectorTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "DLCParsingTestVector"

  it should "have serialization symmetry" in {
    val gen = Gen.oneOf(
      DLCTLVGen.contractInfoParsingTestVector(),
      DLCTLVGen.oracleInfoParsingTestVector(),
      DLCTLVGen.fundingInputParsingTestVector(),
      DLCTLVGen.cetSigsParsingTestVector(),
      DLCTLVGen.fundingSigsParsingTestVector(),
      DLCTLVGen.dlcOfferParsingTestVector(),
      DLCTLVGen.dlcAcceptParsingTestVector(),
      DLCTLVGen.dlcSignParsingTestVector()
    )

    forAll(gen) { parsingTest =>
      val parsingTestResult = DLCParsingTestVector.fromJson(parsingTest.toJson)
      assert(parsingTestResult.isSuccess)
      assert(parsingTestResult.get == parsingTest)
    }
  }

  it should "pass dlc_message_test" in {
    val vecResult = DLCParsingTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(succeed) { case (_, testVec) =>
      val tlv = testVec.input match {
        case tlv: TLV            => tlv
        case msg: LnMessage[TLV] => msg.tlv
        case _: NetworkElement   => fail(s"Could not parse input $testVec")
      }
      assert(DLCParsingTestVector(tlv) == testVec)
    }
  }
}
