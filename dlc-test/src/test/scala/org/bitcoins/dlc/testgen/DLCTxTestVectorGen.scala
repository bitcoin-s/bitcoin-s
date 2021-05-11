package org.bitcoins.dlc.testgen

import java.io.File

import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object DLCTxTestVectorGen
    extends TestVectorGen[DLCTxTestVector, ValidTestInputs] {

  override val defaultTestFile: File = new File(
    "dlc-test/src/test/scala/org/bitcoins/dlc/testgen/dlc_tx_test.json")

  override val testVectorParser: DLCTxTestVector.type = DLCTxTestVector

  override def inputFromJson: JsValue => JsResult[ValidTestInputs] =
    ValidTestInputs.fromJson

  override val inputStr: String = "inputs"

  override def generateFromInput: ValidTestInputs => Future[DLCTxTestVector] = {
    inputs: ValidTestInputs =>
      Future.successful(DLCTxTestVector.fromInputs(inputs))
  }

  override def generateTestVectors(): Future[Vector[DLCTxTestVector]] = {
    val numOutcomesTests = Vector(2, 3, 5, 8).map(DLCTxGen.randomTxTestVector)

    val nonP2WPKHInputTests =
      DLCTxGen.nonP2WPKHInputs.map(DLCTxGen.dlcTxTestVector)

    val numInputs = Vector(1, 2, 3, 8)

    val multiInputTests =
      DLCTxGen.multiInputTests(numInputs).map(DLCTxGen.dlcTxTestVector)

    Future.successful(
      numOutcomesTests ++ nonP2WPKHInputTests ++ multiInputTests)
  }
}
