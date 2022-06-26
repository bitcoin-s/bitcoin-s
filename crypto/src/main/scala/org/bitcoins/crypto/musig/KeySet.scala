package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  ECPublicKey,
  FieldElement,
  NetworkElement,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

/** Represents an ordered set of MuSig signers and their tweaks.
  * This is the data required to (non-interactively) compute the aggPubKey.
  */
trait KeySet {
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

  /** Returns the coefficient of the given key in the aggPubKey sum */
  def keyAggCoef(key: SchnorrPublicKey): FieldElement = {
    if (secondKeyOpt.contains(key)) FieldElement.one
    else {
      val listHashBytes = MuSigUtil.aggListHash(serialize)
      val bytes = MuSigUtil.aggCoefHash(listHashBytes ++ key.bytes)

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

  /** The aggregate public key that represents the n-of-n signers */
  lazy val aggPubKey: ECPublicKey = computeAggPubKeyAndTweakContext._1

  /** Accumulated tweak information */
  lazy val tweakContext: MuSigTweakContext =
    computeAggPubKeyAndTweakContext._2

  /** The first key different from the keys.head,
    * optimized MuSig2 allows this key to have coefficient 1
    */
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

/** The default way of ordering a KeySet is lexicographically */
case class LexicographicKeySet(
    override val keys: Vector[SchnorrPublicKey],
    override val tweaks: Vector[MuSigTweak] = Vector.empty)
    extends KeySet {
  keys.init.zip(keys.tail).foreach { case (key1, key2) =>
    require(key1.hex.compareTo(key2.hex) <= 0,
            "Keys must be sorted lexicographically")
  }
}

/** This represents an arbitrary KeySet, for use in tests.
  * If you have a non-lexicographical order, extend KeySet.
  */
case class UnsortedKeySet(
    override val keys: Vector[SchnorrPublicKey],
    override val tweaks: Vector[MuSigTweak] = Vector.empty)
    extends KeySet
