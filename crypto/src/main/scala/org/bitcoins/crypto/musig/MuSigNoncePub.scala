package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

case class MuSigNoncePub(pubNonces: Vector[SecpPoint]) extends NetworkElement {
  require(pubNonces.length == MuSig2Util.nonceNum)

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

  def sumToKey(b: FieldElement): ECPublicKey = {
    MuSig2Util.nonceSum[SecpPoint](pubNonces,
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

  val infPtBytes: ByteVector = ByteVector.fill(33)(0)

  override def fromBytes(bytes: ByteVector): MuSigNoncePub = {
    val pubs =
      CryptoBytesUtil.splitEvery(bytes, 33).map { b =>
        if (b == infPtBytes) SecpPointInfinity
        else ECPublicKey.fromBytes(b).toPoint
      }

    MuSigNoncePub(pubs)
  }

  def aggregate(nonces: Vector[MuSigNoncePub]): MuSigNoncePub = {
    val aggNonceKeys = 0.until(MuSig2Util.nonceNum).toVector.map { i =>
      nonces.map(multiNonce => multiNonce(i)).reduce(_.add(_))
    }

    MuSigNoncePub(aggNonceKeys)
  }
}
