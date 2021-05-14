package org.bitcoins.dlc.testgen

import java.io.File

import play.api.libs.json._

import scala.concurrent.Future

object DLCTestVectorGen extends TestVectorGen[DLCTestVector, ValidTestInputs] {

  override val defaultTestFile: File = new File(
    "dlc-test/src/test/scala/org/bitcoins/dlc/testgen/dlc_test.json")

  override val testVectorParser: DLCTestVector.type = DLCTestVector

  override def inputFromJson: JsValue => JsResult[ValidTestInputs] =
    ValidTestInputs.fromJson

  override val inputStr: String = "testInputs"

  override def generateFromInput: ValidTestInputs => Future[DLCTestVector] = {
    inputs =>
      Future.fromTry(DLCTxGen.successTestVector(inputs))
  }

  override def generateTestVectors(): Future[Vector[DLCTestVector]] = {
    // Happy Path
    val numOutcomesTests =
      Vector(2, 3, 5, 8, 100).map(DLCTxGen.randomSuccessTestVector)

    val nonP2WPKHInputTests =
      DLCTxGen.nonP2WPKHInputs.map(DLCTxGen.successTestVector(_))

    val multiInputTests = DLCTxGen
      .multiInputTests(Vector(1, 2, 5))
      .map(DLCTxGen.successTestVector(_))

    Future.sequence(
      (numOutcomesTests ++ nonP2WPKHInputTests ++ multiInputTests).map(
        Future.fromTry))
  }
}
