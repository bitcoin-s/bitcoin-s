package org.bitcoins.dlc.testgen

import java.io.File

import org.bitcoins.core.protocol.tlv.EnumOutcome
import play.api.libs.json._

import scala.concurrent.Future

object DLCTestVectorGen
    extends TestVectorGen[
      DLCTestVector[EnumOutcome],
      ValidTestInputs[EnumOutcome]] {

  override val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_test.json")

  override val testVectorParser: DLCTestVectorHelper[EnumOutcome] =
    DLCTestVectorHelper[EnumOutcome]()

  override def inputFromJson: JsValue => JsResult[
    ValidTestInputs[EnumOutcome]] =
    ValidTestInputsHelper[EnumOutcome]().fromJson

  override val inputStr: String = "testInputs"

  override def generateFromInput: ValidTestInputs[EnumOutcome] => Future[
    DLCTestVector[EnumOutcome]] =
    DLCTxGen.successTestVector(_)

  override def generateTestVectors(): Future[
    Vector[DLCTestVector[EnumOutcome]]] = {
    // Happy Path
    val numOutcomesTests =
      Vector(2, 3, 5, 8, 100).map(DLCTxGen.randomSuccessTestVector)

    val nonP2WPKHInputTests =
      DLCTxGen.nonP2WPKHInputs.map(DLCTxGen.successTestVector(_))

    val multiInputTests = DLCTxGen
      .multiInputTests(Vector(1, 2, 5))
      .map(DLCTxGen.successTestVector(_))

    Future.sequence(numOutcomesTests ++ nonP2WPKHInputTests ++ multiInputTests)
  }
}
