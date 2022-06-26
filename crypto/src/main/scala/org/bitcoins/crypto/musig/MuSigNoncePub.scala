package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Wraps the ephemeral points making up a MuSig2 nonce */
case class MuSigNoncePub(pubNonces: Vector[SecpPoint]) extends NetworkElement {
  require(pubNonces.length == MuSigUtil.nonceNum)

  def apply(i: Int): SecpPoint = {
    pubNonces(i)
  }

  def length: Int = pubNonces.length

  override def bytes: ByteVector = {
    pubNonces
      .map {
        case SecpPointInfinity  => MuSigNoncePub.infPtBytes
        case p: SecpPointFinite => p.toPublicKey.bytes
      }
      .reduce(_ ++ _)
  }

  /** Collapses this into a single ephemeral public key */
  def sumToKey(b: FieldElement): ECPublicKey = {
    MuSigUtil.nonceSum[SecpPoint](pubNonces,
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
  val infPtBytes: ByteVector = ByteVector.fill(33)(0)

  override def fromBytes(bytes: ByteVector): MuSigNoncePub = {
    val pubs =
      CryptoBytesUtil.splitEvery(bytes, 33).map { b =>
        if (b == infPtBytes) SecpPointInfinity
        else ECPublicKey.fromBytes(b).toPoint
      }

    MuSigNoncePub(pubs)
  }

  /** Sums the given nonces and returns the aggregate MuSigNoncePub */
  def aggregate(nonces: Vector[MuSigNoncePub]): MuSigNoncePub = {
    val aggNonceKeys = 0.until(MuSigUtil.nonceNum).toVector.map { i =>
      nonces.map(multiNonce => multiNonce(i)).reduce(_.add(_))
    }

    MuSigNoncePub(aggNonceKeys)
  }
}
