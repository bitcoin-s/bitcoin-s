package org.bitcoins.dlc.testgen

import java.io.File

import org.bitcoins.core.protocol.tlv.TLV
import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object DLCParsingTestVectorGen
    extends TestVectorGen[DLCParsingTestVector, TLV] {

  override val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_message_test.json")

  override val testVectorParser: DLCParsingTestVector.type =
    DLCParsingTestVector

  override def inputFromJson: JsValue => JsResult[TLV] =
    DLCParsingTestVector.tlvFromJson

  override val inputStr: String = "input"

  override def generateFromInput: TLV => Future[DLCParsingTestVector] = { tlv =>
    Future.successful(DLCParsingTestVector(tlv))
  }

  override def generateTestVectors(): Future[Vector[DLCParsingTestVector]] = {
    Future.successful(
      Vector(
        DLCTLVGen.contractInfoParsingTestVector(),
        DLCTLVGen.oracleInfoParsingTestVector(),
        DLCTLVGen.fundingInputParsingTestVector(),
        DLCTLVGen.cetSigsParsingTestVector(),
        DLCTLVGen.fundingSigsParsingTestVector(),
        DLCTLVGen.dlcOfferParsingTestVector(),
        DLCTLVGen.dlcAcceptParsingTestVector(),
        DLCTLVGen.dlcSignParsingTestVector()
      )
    )
  }
}
