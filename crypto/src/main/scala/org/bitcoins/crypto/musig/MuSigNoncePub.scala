package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Wraps the ephemeral points making up a MuSig2 nonce */
case class MuSigNoncePub(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 66,
          s"FrostNoncePub must be 66 bytes, got: ${bytes.length}")

  private def isValidPubKeys: Boolean = {
    bytes
      .grouped(33)
      .forall(b =>
        b == SecpPointInfinity.bytes || CryptoUtil.isValidPubKey(
          ECPublicKeyBytes(b)))

  }

  /** Helper function to parse [[SecpPointInfinity]] case as that is not a valid
    * public key
    */
  require(
    isValidPubKeys,
    s"Each 33-byte slice of MuSigNoncePub must be either the point at infinity or a valid compressed public key, got: ${bytes.grouped(33).toVector.map(_.toHex)}"
  )

  private def parse(bytes: ByteVector): SecpPoint = {
    SecpPoint.fromBytes(bytes)
  }
  val r1: SecpPoint = {
    parse(bytes.take(33))
  }

  val r2: SecpPoint = {
    parse(bytes.takeRight(33))
  }

  /** Collapses this into a single ephemeral public key */
  def sumToKey(b: FieldElement): ECPublicKey = {
    MuSigUtil.nonceSum[SecpPoint](Vector(r1, r2),
                                  b,
                                  _.add(_),
                                  _.multiply(_),
                                  SecpPointInfinity) match {
      case SecpPointInfinity  => CryptoParams.getG
      case p: SecpPointFinite => p.toPublicKey
    }
  }
}

object MuSigNoncePub extends Factory[MuSigNoncePub] {

  /** In the BIP, the point at infinity is serialized as 33 0x00 bytes */
  val infPtBytes: ByteVector = ByteVector.low(33)

  override def fromBytes(bytes: ByteVector): MuSigNoncePub = {
    new MuSigNoncePub(bytes)
  }

  def apply(r1: SecpPoint, r2: SecpPoint): MuSigNoncePub = {
    MuSigNoncePub(r1.bytes ++ r2.bytes)
  }

  def apply(key1: ECPublicKey, key2: ECPublicKey): MuSigNoncePub = {
    require(key1.isCompressed && key2.isCompressed)
    MuSigNoncePub(key1.bytes ++ key2.bytes)
  }
}
