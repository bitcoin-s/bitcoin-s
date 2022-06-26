package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  ECPublicKey,
  FieldElement,
  NetworkElement,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

sealed trait KeySet {
  def keys: Vector[SchnorrPublicKey]

  def tweaks: Vector[MuSigTweak]

  def withTweaks(newTweaks: Vector[MuSigTweak]): KeySet = {
    require(tweaks.isEmpty, "withTweaks is not meant for replacing tweaks")

    this match {
      case ks: LexicographicKeySet => ks.copy(tweaks = newTweaks)
      case ks: UnsortedKeySet      => ks.copy(tweaks = newTweaks)
    }
  }

  def length: Int = keys.length

  def apply(i: Int): SchnorrPublicKey = {
    keys(i)
  }

  lazy val serialize: ByteVector = {
    keys.map(_.bytes).reduce(_ ++ _)
  }

  def keyAggCoef(key: SchnorrPublicKey): FieldElement = {
    if (secondKeyOpt.contains(key)) FieldElement.one
    else {
      val listHashBytes = MuSig2Util.aggListHash(serialize)
      val bytes = MuSig2Util.aggCoefHash(listHashBytes ++ key.bytes)

      FieldElement(new java.math.BigInteger(1, bytes.toArray))
    }
  }

  private lazy val computeAggPubKeyAndTweakContext: (
      ECPublicKey,
      MuSigTweakContext) = {
    val untweakedAggPubKey = keys
      .map { key =>
        val coef = keyAggCoef(key)
        key.publicKey.multiply(coef)
      }
      .reduce(_.add(_))

    tweaks.foldLeft((untweakedAggPubKey, MuSigTweakContext.empty)) {
      case ((pubKeySoFar, context), tweak) =>
        context.applyTweak(tweak, pubKeySoFar)
    }
  }

  lazy val aggPubKey: ECPublicKey = computeAggPubKeyAndTweakContext._1

  lazy val tweakContext: MuSigTweakContext =
    computeAggPubKeyAndTweakContext._2

  // In truth this represents the first key different from the head key
  lazy val secondKeyOpt: Option[SchnorrPublicKey] = {
    keys.find(_ != keys.head)
  }
}

object KeySet {

  def apply(keys: Vector[SchnorrPublicKey]): LexicographicKeySet = {
    val sortedKeys = keys.sorted(NetworkElement.lexicographicalOrdering)
    LexicographicKeySet(sortedKeys)
  }

  def apply(keys: SchnorrPublicKey*): LexicographicKeySet = {
    KeySet(keys.toVector)
  }

  def apply(
      keys: Vector[SchnorrPublicKey],
      tweaks: Vector[MuSigTweak]): LexicographicKeySet = {
    val sortedKeys = keys.sorted(NetworkElement.lexicographicalOrdering)
    LexicographicKeySet(sortedKeys, tweaks)
  }
}

case class LexicographicKeySet(
    override val keys: Vector[SchnorrPublicKey],
    override val tweaks: Vector[MuSigTweak] = Vector.empty)
    extends KeySet {
  keys.init.zip(keys.tail).foreach { case (key1, key2) =>
    require(key1.hex.compareTo(key2.hex) <= 0,
            "Keys must be sorted lexicographically")
  }
}

case class UnsortedKeySet(
    override val keys: Vector[SchnorrPublicKey],
    override val tweaks: Vector[MuSigTweak] = Vector.empty)
    extends KeySet
