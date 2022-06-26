package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

case class MuSigNoncePriv(privNonces: Vector[ECPrivateKey])
    extends NetworkElement {
  require(privNonces.length == MuSig2Util.nonceNum)

  def toPublicNonces: MuSigNoncePub = {
    MuSigNoncePub(privNonces.map(_.publicKey.toPoint))
  }

  def toFieldElements: Vector[FieldElement] = {
    privNonces.map(_.fieldElement)
  }

  def length: Int = privNonces.length

  override def bytes: ByteVector = {
    privNonces.map(_.bytes).reduce(_ ++ _)
  }

  def negate: MuSigNoncePriv = {
    MuSigNoncePriv(privNonces.map(_.negate))
  }

  def sumToKey(b: FieldElement): FieldElement = {
    MuSig2Util.nonceSum[FieldElement](toFieldElements,
                                      b,
                                      _.add(_),
                                      _.multiply(_),
                                      FieldElement.zero)
  }
}

object MuSigNoncePriv extends Factory[MuSigNoncePriv] {

  override def fromBytes(bytes: ByteVector): MuSigNoncePriv = {
    val privs =
      CryptoBytesUtil.splitEvery(bytes, 32).map(ECPrivateKey.fromBytes)
    MuSigNoncePriv(privs)
  }

  // TODO change aggPubKey back to SchnorrPublicKey and remove requirement once test vector is changed to valid x-coordinate
  def genInternal(
      preRand: ByteVector,
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[ByteVector] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    require(preRand.length == 32)
    require(msgOpt.forall(msg => msg.length == 32))
    require(aggPubKeyOpt.forall(aggPubKey => aggPubKey.length == 32))
    require(extraInOpt.forall(_.length <= 4294967295L))

    def serializeWithLen(
        bytesOpt: Option[ByteVector],
        lengthSize: Int = 1): ByteVector = {
      bytesOpt match {
        case Some(bytes) =>
          ByteVector.fromLong(bytes.length, lengthSize) ++ bytes
        case None => ByteVector.fromLong(0, lengthSize)
      }
    }

    val rand = privKeyOpt match {
      case Some(privKey) => MuSig2Util.auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val aggPubKeyBytes = serializeWithLen(aggPubKeyOpt)
    val msgBytes = serializeWithLen(msgOpt)
    val extraInBytes = serializeWithLen(extraInOpt, lengthSize = 4)
    val dataBytes = rand ++ aggPubKeyBytes ++ msgBytes ++ extraInBytes

    val privNonceKeys = 0.until(MuSig2Util.nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)
      val noncePreBytes = MuSig2Util.nonHash(dataBytes ++ indexByte)
      val noncePreNum = new java.math.BigInteger(1, noncePreBytes.toArray)

      FieldElement(noncePreNum).toPrivateKey
    }

    MuSigNoncePriv(privNonceKeys)
  }

  def gen(
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    val preRand = CryptoUtil.randomBytes(32)

    genInternal(preRand,
                privKeyOpt,
                aggPubKeyOpt.map(_.bytes),
                msgOpt,
                extraInOpt)
  }
}
