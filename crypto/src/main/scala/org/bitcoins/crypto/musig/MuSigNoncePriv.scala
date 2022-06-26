package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Wraps the ephemeral private keys making up a MuSig2 nonce */
case class MuSigNoncePriv(privNonces: Vector[ECPrivateKey])
    extends NetworkElement {
  require(privNonces.length == MuSigUtil.nonceNum,
          s"Exactly ${MuSigUtil.nonceNum} keys are expected, found $privNonces")

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

  /** Collapses this into a single ephemeral private key */
  def sumToKey(b: FieldElement): FieldElement = {
    MuSigUtil.nonceSum[FieldElement](toFieldElements,
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

  /** Generates a MuSigNoncePriv given 32 bytes of entropy from preRand,
    * and possibly some other sources, as specified in the BIP.
    */
  def genInternal(
      preRand: ByteVector,
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    require(preRand.length == 32,
            s"32 bytes of entropy must be provided, found $preRand")
    require(msgOpt.forall(msg => msg.length == 32),
            s"The message to be signed must be 32 bytes, found $msgOpt")
    require(
      extraInOpt.forall(_.length <= 4294967295L),
      "extraIn too long, its length must be represented by at most four bytes")

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
      case Some(privKey) => MuSigUtil.auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val aggPubKeyBytes = serializeWithLen(aggPubKeyOpt.map(_.bytes))
    val msgBytes = serializeWithLen(msgOpt)
    val extraInBytes = serializeWithLen(extraInOpt, lengthSize = 4)
    val dataBytes = rand ++ aggPubKeyBytes ++ msgBytes ++ extraInBytes

    val privNonceKeys = 0.until(MuSigUtil.nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)
      val noncePreBytes = MuSigUtil.nonHash(dataBytes ++ indexByte)
      val noncePreNum = new java.math.BigInteger(1, noncePreBytes.toArray)

      FieldElement(noncePreNum).toPrivateKey
    }

    MuSigNoncePriv(privNonceKeys)
  }

  /** Generates 32 bytes of entropy and contructs a MuSigNoncePriv from this,
    * and possibly some other sources, as specified in the BIP.
    */
  def gen(
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    val preRand = CryptoUtil.randomBytes(32)

    genInternal(preRand, privKeyOpt, aggPubKeyOpt, msgOpt, extraInOpt)
  }
}
