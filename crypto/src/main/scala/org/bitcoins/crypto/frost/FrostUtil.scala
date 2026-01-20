package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{
  CryptoParams,
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  XOnlyPubKey
}
import scodec.bits.ByteVector

object FrostUtil {

  def hashFrostAux(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/aux").bytes
  }

  def hashFrostNonce(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/nonce").bytes
  }

  def nonceGen(
      rand: ByteVector,
      secshare: Option[ByteVector],
      pubshare: Option[ECPublicKey],
      threshold_pk: Option[XOnlyPubKey],
      message: Option[ByteVector],
      extra_in: Option[ByteVector]): (ByteVector, ByteVector) = {
    val randPrime = secshare match {
      case Some(sec) => sec.xor(hashFrostAux(rand))
      case None      => rand
    }

    // Match the Python reference: None -> 0x00, Some(msg) -> 0x01 || len(msg,8) || msg
    // Note: an explicit empty message (Some(ByteVector.empty)) must be encoded as
    // 0x01 followed by 8 zero bytes (length 0), which differs from None.
    val mPrefix = message match {
      case Some(m) =>
        ByteVector.fromByte(1) ++ ByteVector.fromLong(m.length, 8) ++ m
      case None => ByteVector.fromByte(0)
    }

    val preimages: Vector[FieldElement] = 0
      .until(2)
      .map { i =>
        val b = randPrime ++
          ByteVector.fromLong(pubshare.map(_.bytes.size).getOrElse(0), 1) ++
          pubshare.map(_.bytes).getOrElse(ByteVector.empty) ++
          ByteVector.fromLong(threshold_pk.map(_.bytes.size).getOrElse(0), 1) ++
          threshold_pk.map(_.bytes).getOrElse(ByteVector.empty) ++
          mPrefix ++
          ByteVector.fromLong(extra_in.map(_.length).getOrElse(0), 4) ++
          extra_in.getOrElse(ByteVector.empty) ++
          ByteVector.fromByte(i.toByte)
        val hash = hashFrostNonce(b)
        FieldElement.fromBytes(hash)
      }
      .toVector
    val r1: ECPublicKey = CryptoParams.getG.multiply(preimages.head)
    val r2: ECPublicKey = CryptoParams.getG.multiply(preimages(1))
    val pubnonce = r1.bytes ++ r2.bytes
    val secnonce = preimages.head.bytes ++ preimages(1).bytes
    (secnonce, pubnonce)
  }
}
