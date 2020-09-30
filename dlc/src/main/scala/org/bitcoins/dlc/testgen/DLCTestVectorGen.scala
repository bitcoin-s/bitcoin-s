package org.bitcoins.dlc.testgen

import java.io.File

import play.api.libs.json._

import scala.concurrent.Future

object DLCTestVectorGen extends TestVectorGen[DLCTestVector, ValidTestInputs] {

  override val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_test.json")

  override val testVectorParser: DLCTestVector.type = DLCTestVector

  override def inputFromJson: JsValue => JsResult[ValidTestInputs] =
    ValidTestInputs.fromJson

  override val inputStr: String = "testInputs"

  override def generateFromInput: ValidTestInputs => Future[DLCTestVector] =
    DLCTxGen.successTestVector(_)

  override def generateTestVectors(): Future[Vector[DLCTestVector]] = {
    // Happy Path
    Future.sequence(
      Vector(2, 3, 5, 8, 100).map(DLCTxGen.randomSuccessTestVector))
  }
}
