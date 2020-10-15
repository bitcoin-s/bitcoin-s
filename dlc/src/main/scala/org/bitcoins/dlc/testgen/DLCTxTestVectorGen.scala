package org.bitcoins.dlc.testgen

import java.io.File

import org.bitcoins.core.protocol.tlv.EnumOutcome
import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object DLCTxTestVectorGen
    extends TestVectorGen[
      DLCTxTestVector[EnumOutcome],
      ValidTestInputs[EnumOutcome]] {

  override val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_tx_test.json")

  override val testVectorParser: DLCTxTestVectorHelper[EnumOutcome] =
    DLCTxTestVectorHelper[EnumOutcome]()

  override def inputFromJson: JsValue => JsResult[
    ValidTestInputs[EnumOutcome]] =
    ValidTestInputsHelper[EnumOutcome]().fromJson

  override val inputStr: String = "inputs"

  override def generateFromInput: ValidTestInputs[EnumOutcome] => Future[
    DLCTxTestVector[EnumOutcome]] =
    DLCTxTestVectorHelper[EnumOutcome]().fromInputs

  override def generateTestVectors(): Future[
    Vector[DLCTxTestVector[EnumOutcome]]] = {
    val numOutcomesTests = Vector(2, 3, 5, 8).map(DLCTxGen.randomTxTestVector)

    val nonP2WPKHInputTests =
      DLCTxGen.nonP2WPKHInputs.map(DLCTxGen.dlcTxTestVector(_))

    val numInputs = Vector(1, 2, 3, 8)

    val multiInputTests =
      DLCTxGen.multiInputTests(numInputs).map(DLCTxGen.dlcTxTestVector(_))

    Future.sequence(numOutcomesTests ++ nonP2WPKHInputTests ++ multiInputTests)
  }
}
