package org.bitcoins.crypto

import scodec.bits.ByteVector

case class PresigVector(
    secretKey: Option[ECPrivateKey],
    publicKey: XOnlyPubKey,
    auxRand: Option[ByteVector],
    message: Option[ByteVector],
    adaptor: ECPublicKey,
    preSignature: SchnorrAdaptorSignature,
    result: Boolean,
    comment: String
)

object PresigVector {
  def fromCsvLine(line: String): PresigVector = {
    val split = line.split(",")
    // index,secret key,public key,aux_rand,message,adaptor,pre-signature,result,comment
    val secretKeyOpt =
      if (split(1).isEmpty) None else Some(ECPrivateKey(split(1)))
    val publicKey = XOnlyPubKey(split(2))
    val auxRandOpt =
      if (split(3).isEmpty) None else Some(ByteVector.fromValidHex(split(3)))
    val messageOpt =
      if (split(4).isEmpty) None else Some(ByteVector.fromValidHex(split(4)))
    val adaptor = ECPublicKey(split(5))
    val preSig = SchnorrAdaptorSignature(ByteVector.fromValidHex(split(6)))
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
