package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  ECPublicKey,
  EvenParity,
  FieldElement,
  KeyParity,
  OddParity
}

/** Represents either FieldElement.one or FieldElement.orderMinusOne.
  * Using this ADT rather than those actual FieldElements saves computation
  * including some unnecessary point multiplications.
  *
  * In general there is a correspondence between Pos <-> EvenParity and Neg <-> OddParity,
  * this is because in general x-only keys are assumed to be even and need to be negated
  * if they are meant to be used as odd keys.
  */
sealed trait ParityMultiplier {

  def modify(fieldElement: FieldElement): FieldElement = {
    this match {
      case Pos => fieldElement
      case Neg => fieldElement.negate
    }
  }

  def modify(pubKey: ECPublicKey): ECPublicKey = {
    this match {
      case Pos => pubKey
      case Neg => pubKey.negate
    }
  }

  /** Combines two ParityMultiplier into a single one representing their net modification */
  def multiply(other: ParityMultiplier): ParityMultiplier = {
    (this, other) match {
      case (Pos, Pos) | (Neg, Neg) => Pos
      case (Pos, Neg) | (Neg, Pos) => Neg
    }
  }

  def toParity: KeyParity = {
    this match {
      case Pos => EvenParity
      case Neg => OddParity
    }
  }
}

object ParityMultiplier {

  def fromParity(keyParity: KeyParity): ParityMultiplier = {
    keyParity match {
      case EvenParity => Pos
      case OddParity  => Neg
    }
  }
}

/** Represents FieldElement.one */
case object Pos extends ParityMultiplier

/** Represents FieldElement.orderMinusOne */
case object Neg extends ParityMultiplier
