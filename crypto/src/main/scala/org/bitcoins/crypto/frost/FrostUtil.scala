package org.bitcoins.crypto.frost

import org.bitcoins.crypto.CryptoUtil
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
    val rand = secshare.xor(hashFrostAux(rand))
    val mPrefix = if (message.isEmpty) {
      ByteVector.fromByte(0)
    } else {
      ByteVector.fromByte(1) ++ ByteVector.fromLong(message.length, 8) ++ message
    }

    val preimages: Vector[ByteVector] = 0.until(2).map { i =>
      rand ++ ByteVector.fromLong(pubshare.length, 1) ++ pubshare ++
        ByteVector.fromLong(threshold_pk.length, 1) ++ threshold_pk ++ mPrefix ++
        extra_in.getOrElse(ByteVector.empty) ++ ByteVector.fromByte(i.toByte)
    }.toVector
    ???
  }
}
