package org.bitcoins.crypto.frost
import org.bitcoins.crypto.*
import scodec.bits.ByteVector

case class FrostSessionContext(
    signingContext: FrostSigningContext,
    aggNonce: FrostNoncePub,
    tweaks: Vector[FieldElement],
    isXOnly: Vector[Boolean],
    message: ByteVector) {
  def v: Long = tweaks.length

  require(v >= 0 && v < 4294967296L,
          s"v must be in the range [0, 2^32 - 1], got: $v")

  require(isXOnly.length == v.toInt,
          s"isXOnly must have the same length as tweaks (v=$v)")

  def r1: SecpPoint = aggNonce.r1
  def r2: SecpPoint = aggNonce.r2

  def getSessionValues: FrostSessionValues = {
    val thresholdPk = signingContext.thresholdPubKey
    val initTweakCtx = FrostTweakContext(thresholdPk)
    val tweakCtx =
      1.to(v.toInt).foldLeft(initTweakCtx) { case (tweakCtx, i) =>
        tweakCtx.applyTweak(tweaks(i - 1), isXOnly(i - 1))
      }
    val serializedIds =
      signingContext.participantIds.sorted.foldLeft(ByteVector.empty) {
        case (acc, id) =>
          acc ++ ByteVector.fromLong(id, 4)
      }

    val bHash = FrostUtil.hashFrostNonceCoef(
      serializedIds ++
        aggNonce.bytes ++
        tweakCtx.getXOnlyPubKey.bytes ++
        message
    )
    val b = FieldElement.fromBytes(bHash)

    require(b != FieldElement.zero,
            s"Computed challenge 'b' cannot be zero in FROST signing session")
    // come back and looks at this to make sure we handle
    // infinity correct in the getsessionvalues algo ('cpoint_ext')
    val rPrime = r1.add(r2.multiply(b))
    val r = rPrime match {
      case SecpPointInfinity  => CryptoParams.getG
      case p: SecpPointFinite => p.toPublicKey
    }
    val eHash = CryptoUtil.sha256SchnorrChallenge(
      r.toXOnly.bytes ++
        tweakCtx.getXOnlyPubKey.bytes ++
        message
    )
    val e = FieldElement.fromBytes(eHash.bytes)
    require(e != FieldElement.zero,
            s"Computed challenge 'e' cannot be zero in FROST signing session")
    FrostSessionValues(tweakCtx = tweakCtx,
                       ids = signingContext.participantIds,
                       pubshares = signingContext.pubshares,
                       b = b,
                       R = r.toPoint,
                       e = e)
  }
}
