package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{CryptoParams, CryptoUtil, ECPublicKey, FieldElement}
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
      secshare: ByteVector,
      pubshare: ByteVector,
      threshold_pk: ByteVector,
      message: ByteVector,
      extra_in: Option[ByteVector]): (ByteVector, ByteVector) = {
    val randPrime = secshare.xor(hashFrostAux(rand))
    val mPrefix = if (message.isEmpty) {
      ByteVector.fromByte(0)
    } else {
      ByteVector.fromByte(1) ++ ByteVector.fromLong(message.length,
                                                    8) ++ message
    }

    val preimages: Vector[FieldElement] = 0
      .until(2)
      .map { i =>
        val b = randPrime ++
          ByteVector.fromLong(pubshare.length, 1) ++
          pubshare ++
          ByteVector.fromLong(threshold_pk.length, 1) ++ threshold_pk ++
          mPrefix ++
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
