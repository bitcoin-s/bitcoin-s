package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{ECPublicKey, FieldElement, NetworkElement}
import scodec.bits.ByteVector

/** Represents an ordered set of MuSig signers and their tweaks. This is the
  * data required to (non-interactively) compute the aggPubKey.
  */
sealed trait KeySet {
  def keys: Vector[ECPublicKey]

  def tweaks: Vector[MuSigTweak]

  def withTweaks(newTweaks: Vector[MuSigTweak]): KeySet = {
    require(tweaks.isEmpty, "withTweaks is not meant for replacing tweaks")

    this match {
      case ks: LexicographicKeySet => ks.copy(tweaks = newTweaks)
      case ks: UnsortedKeySet      => ks.copy(tweaks = newTweaks)
    }
  }

  def length: Int = keys.length

  def apply(i: Int): ECPublicKey = {
    keys(i)
  }

  lazy val serialize: ByteVector = {
    keys.map(_.bytes).reduce(_ ++ _)
  }

  /** Returns the coefficient of the given key in the aggPubKey sum */
  def keyAggCoef(publicKeyI: ECPublicKey): FieldElement = {
    if (secondKeyOpt.contains(publicKeyI)) FieldElement.one
    else {
      val listHashBytes = MuSigUtil.aggListHash(serialize)
      val bytes = MuSigUtil.aggCoefHash(listHashBytes ++ publicKeyI.bytes)

      FieldElement.fromBytes(bytes)
    }
  }

  def getSessionKeyAggCoeff(
      signingSession: MuSigSessionContext,
      key: ECPublicKey): FieldElement = {
    if (signingSession.keySet.keys.contains(key)) {
      val coeff = keyAggCoef(key)
      coeff
    } else {
      throw new IllegalArgumentException(
        s"Key ${key.hex} not found in signing session key set")
    }
  }

  /** The aggregate public key that represents the n-of-n signers */
  lazy val aggPubKey: ECPublicKey =
    tweakContext.Q.toPublicKey

  /** Accumulated tweak information */
  lazy val tweakContext: MuSigTweakContext = {
    val untweakedAggPubKey = keys
      .map { key =>
        val coef = keyAggCoef(key)
        key.multiply(coef)
      }
      .reduce(_.add(_))

    tweaks.foldLeft(MuSigTweakContext(untweakedAggPubKey)) {
      case (context, tweak) =>
        context.applyTweak(tweak)
    }
  }

  /** The first key different from the keys.head, optimized MuSig2 allows this
    * key to have coefficient 1
    */
  lazy val secondKeyOpt: Option[ECPublicKey] = {
    keys.find(_ != keys.head)
  }
}

object KeySet {

//  def apply(keys: Vector[SchnorrPublicKey]): LexicographicKeySet = {
//    val sortedKeys = keys.sorted(using NetworkElement.lexicographicalOrdering)
//    LexicographicKeySet(sortedKeys)
//  }

  def apply(
      keys: Vector[ECPublicKey],
      tweaks: Vector[MuSigTweak]): LexicographicKeySet = {
    fromUnsorted(keys, tweaks)
  }

  def fromUnsorted(keys: Vector[ECPublicKey]): LexicographicKeySet = {
    fromUnsorted(keys, Vector.empty)
  }

  def fromUnsorted(
      keys: Vector[ECPublicKey],
      tweaks: Vector[MuSigTweak]): LexicographicKeySet = {
    val sortedKeys = keys.sorted(using NetworkElement.lexicographicalOrdering)
    LexicographicKeySet(sortedKeys, tweaks)
  }

  def fromUnsorted(unsortedKeySet: UnsortedKeySet): LexicographicKeySet = {
    fromUnsorted(unsortedKeySet.keys, unsortedKeySet.tweaks)
  }
}

/** The default way of ordering a KeySet is lexicographically */
case class LexicographicKeySet(
    override val keys: Vector[ECPublicKey],
    override val tweaks: Vector[MuSigTweak] = Vector.empty)
    extends KeySet {
  keys.init.zip(keys.tail).foreach { case (key1, key2) =>
    require(key1.hex.compareTo(key2.hex) <= 0,
            "Keys must be sorted lexicographically")
  }
}

/** This represents an arbitrary KeySet, for use in tests. If you have a
  * non-lexicographical order, extend KeySet.
  */
case class UnsortedKeySet(
    override val keys: Vector[ECPublicKey],
    override val tweaks: Vector[MuSigTweak] = Vector.empty)
    extends KeySet {
  def toSorted: LexicographicKeySet = KeySet.fromUnsorted(this)
}
