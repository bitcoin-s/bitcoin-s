package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.util.Try

case class PresigVector(
    secretKey: Option[ECPrivateKey],
    publicKey: Try[XOnlyPubKey],
    auxRand: Option[ByteVector],
    message: Option[ByteVector],
    adaptor: Try[ECPublicKey],
    preSignature: Try[SchnorrAdaptorSignature],
    result: Boolean,
    comment: String
)

object PresigVector {
  def fromCsvLine(line: String): PresigVector = {
    val split = line.split(",")
    // index,secret key,public key,aux_rand,message,adaptor,pre-signature,result,comment
    val secretKeyOpt =
      if (split(1).isEmpty) None else Some(ECPrivateKey(split(1)))
    val publicKey = XOnlyPubKey.fromHexT(split(2))
    val auxRandOpt =
      if (split(3).isEmpty) None else Some(ByteVector.fromValidHex(split(3)))
    val messageOpt =
      if (split(4).isEmpty) None else Some(ByteVector.fromValidHex(split(4)))
    val adaptor = ECPublicKey.fromHexT(split(5))
    val preSig =
      SchnorrAdaptorSignature.fromBytesT(ByteVector.fromValidHex(split(6)))
    val result = split(7).toBoolean
    val comment = if (split.length > 8) split(8) else ""

    PresigVector(secretKeyOpt,
                 publicKey,
                 auxRandOpt,
                 messageOpt,
                 adaptor,
                 preSig,
                 result,
                 comment)
  }
}

case class AdaptVector(
    publicKey: Try[SchnorrPublicKey],
    message: ByteVector,
    adaptorSecret: Try[ECPrivateKey],
    preSignature: Try[SchnorrAdaptorSignature],
    signature: Try[SchnorrDigitalSignature],
    result: Boolean,
    comment: String
)

object AdaptVector {
  def fromCsvLine(line: String): AdaptVector = {
    val split = line.split(",", -1)
    // index,pubkey,message,secadaptor,pre-signature,BIP340 signature,result,comment
    val publicKeyStr = split(1)
    val messageStr = split(2)
    val adaptorSecretStr = split(3)
    val preSignatureStr = split(4)
    val signatureStr = split(5)
    val resultStr = split(6)
    val comment = if (split.length > 7) split(7) else ""

    val publicKey = Try(SchnorrPublicKey(publicKeyStr))
    val message = ByteVector.fromValidHex(messageStr)
    val adaptorSecret = Try(ECPrivateKey(adaptorSecretStr))
    val preSignature =
      Try(SchnorrAdaptorSignature(ByteVector.fromValidHex(preSignatureStr)))
    val signature =
      Try(SchnorrDigitalSignature(ByteVector.fromValidHex(signatureStr)))
    val result = resultStr.toUpperCase == "TRUE"

    AdaptVector(publicKey,
                message,
                adaptorSecret,
                preSignature,
                signature,
                result,
                comment)
  }
}

case class SecAdaptorVector(
    preSignature: Try[SchnorrAdaptorSignature],
    signature: Try[SchnorrDigitalSignature],
    adaptorSecret: Try[ECPrivateKey],
    result: Boolean,
    comment: String
)

object SecAdaptorVector {
  def fromCsvLine(line: String): SecAdaptorVector = {
    val split = line.split(",", -1)
    val preSignatureStr = split(1)
    val signatureStr = split(2)
    val adaptorSecretStr = split(3)
    val resultStr = split(4)
    val comment = if (split.length > 5) split(5) else ""

    val preSignature =
      Try(SchnorrAdaptorSignature(ByteVector.fromValidHex(preSignatureStr)))
    val signature =
      Try(SchnorrDigitalSignature(ByteVector.fromValidHex(signatureStr)))
    val adaptorSecret = Try(ECPrivateKey(adaptorSecretStr))
    val result = resultStr.toUpperCase == "TRUE"

    SecAdaptorVector(preSignature, signature, adaptorSecret, result, comment)
  }
}
