package org.bitcoins.dlc.testgen

import java.io.File

import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object DLCTxTestVectorGen
    extends TestVectorGen[DLCTxTestVector, ValidTestInputs] {

  override val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_tx_test.json")

  override val testVectorParser: DLCTxTestVector.type = DLCTxTestVector

  override def inputFromJson: JsValue => JsResult[ValidTestInputs] =
    ValidTestInputs.fromJson

  override val inputStr: String = "inputs"

  override def generateFromInput: ValidTestInputs => Future[DLCTxTestVector] =
    DLCTxTestVector.fromInputs

  override def generateTestVectors(): Future[Vector[DLCTxTestVector]] = {
    Future.sequence(Vector(2, 3, 5, 8).map(DLCTxGen.randomTxTestVector))
  }
}
