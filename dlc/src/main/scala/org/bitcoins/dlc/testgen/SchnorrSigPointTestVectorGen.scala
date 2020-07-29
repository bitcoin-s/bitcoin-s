package org.bitcoins.dlc.testgen

import java.io.File

import org.bitcoins.crypto.{ECPrivateKey, Sha256Digest}
import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object SchnorrSigPointTestVectorGen
    extends TestVectorGen[
      SchnorrSigPointTestVector,
      SchnorrSigPointTestVectorInput] {

  override val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_schnorr_test.json")

  override val testVectorParser: SchnorrSigPointTestVector.type =
    SchnorrSigPointTestVector

  override def inputFromJson: JsValue => JsResult[
    SchnorrSigPointTestVectorInput] =
    SchnorrSigPointTestVectorInput.fromJson

  override val inputStr: String = "inputs"

  override def generateFromInput: SchnorrSigPointTestVectorInput => Future[
    SchnorrSigPointTestVector] = { inputs =>
    Future.successful(SchnorrSigPointTestVector(inputs))
  }

  override def generateTestVectors(): Future[
    Vector[SchnorrSigPointTestVector]] = {
    def generateTest: SchnorrSigPointTestVector = {
      SchnorrSigPointTestVector(
        ECPrivateKey.freshPrivateKey,
        ECPrivateKey.freshPrivateKey,
        Sha256Digest(ECPrivateKey.freshPrivateKey.bytes)
      )
    }

    Future.successful(
      Vector.fill(5)(generateTest)
    )
  }
}
