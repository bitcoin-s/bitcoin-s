package org.bitcoins.dlc.testgen

import org.bitcoins.crypto._
import play.api.libs.json._

case class SchnorrSigPointTestVector(
    inputs: SchnorrSigPointTestVectorInput,
    pubKey: SchnorrPublicKey,
    pubNonce: SchnorrNonce,
    signature: SchnorrDigitalSignature,
    sigPoint: ECPublicKey)
    extends TestVector {
  require(signature.sig.getPublicKey == sigPoint,
          s"Signature ($signature) does not match Signature Point ($sigPoint)")

  def privKey: ECPrivateKey = inputs.privKey
  def privNonce: ECPrivateKey = inputs.privNonce
  def msgHash: Sha256Digest = inputs.msgHash

  override def toJson: JsValue = {
    Json.toJson(this)(SchnorrSigPointTestVector.schnorrSigPointTestVectorFormat)
  }
}

case class SchnorrSigPointTestVectorInput(
    privKey: ECPrivateKey,
    privNonce: ECPrivateKey,
    msgHash: Sha256Digest)

object SchnorrSigPointTestVectorInput {

  def fromJson(json: JsValue): JsResult[SchnorrSigPointTestVectorInput] = {
    json.validate[SchnorrSigPointTestVectorInput](
      SchnorrSigPointTestVector.schnorrSigPointTestVectorInputFormat)
  }
}

object SchnorrSigPointTestVector
    extends TestVectorParser[SchnorrSigPointTestVector] {

  def apply(
      input: SchnorrSigPointTestVectorInput): SchnorrSigPointTestVector = {
    SchnorrSigPointTestVector(input.privKey, input.privNonce, input.msgHash)
  }

  def apply(
      privKey: ECPrivateKey,
      privNonce: ECPrivateKey,
      msgHash: Sha256Digest): SchnorrSigPointTestVector = {
    val signature = privKey.schnorrSignWithNonce(msgHash.bytes, privNonce)
    val signaturePoint = signature.sig.getPublicKey

    SchnorrSigPointTestVector(
      SchnorrSigPointTestVectorInput(privKey, privNonce, msgHash),
      privKey.schnorrPublicKey,
      privNonce.schnorrNonce,
      signature,
      signaturePoint)
  }

  def networkElementFormat[T <: NetworkElement](
      factory: Factory[T]): Format[T] = {
    Format[T]({ json =>
                json.validate[String].map(factory.fromHex)
              },
              { element =>
                JsString(element.hex)
              })
  }

  implicit val privKeyFormat: Format[ECPrivateKey] = networkElementFormat(
    ECPrivateKey)

  implicit val schnorrPubKeyFormat: Format[SchnorrPublicKey] =
    networkElementFormat(SchnorrPublicKey)

  implicit val schnorrNonceFormat: Format[SchnorrNonce] = networkElementFormat(
    SchnorrNonce)

  implicit val hashFormat: Format[Sha256Digest] = networkElementFormat(
    Sha256Digest)

  implicit val signatureFormat: Format[SchnorrDigitalSignature] =
    networkElementFormat(SchnorrDigitalSignature)

  implicit val pubKeyFromat: Format[ECPublicKey] = networkElementFormat(
    ECPublicKey)

  implicit val schnorrSigPointTestVectorInputFormat: Format[
    SchnorrSigPointTestVectorInput] =
    Json.format[SchnorrSigPointTestVectorInput]

  implicit val schnorrSigPointTestVectorFormat: Format[
    SchnorrSigPointTestVector] = Json.format[SchnorrSigPointTestVector]

  override def fromJson(json: JsValue): JsResult[SchnorrSigPointTestVector] = {
    json.validate[SchnorrSigPointTestVector]
  }
}
