package org.bitcoins.core.dlc.testgen

import java.io.File

import org.bitcoins.crypto.{ECPrivateKey, Sha256Digest}
import play.api.libs.json.{JsResult, JsValue}

import scala.concurrent.Future

object SchnorrSigPointTestVectorGen
    extends TestVectorGen[
      SchnorrSigPointTestVector,
      SchnorrSigPointTestVectorInput] {

  override val defaultTestFile: File = new File(
    "core-test/.jvm/src/test/scala/org/bitcoins/core/dlc/testgen/dlc_schnorr_test.json")

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
