package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  ECPublicKey,
  EvenParity,
  FieldElement,
  KeyParity,
  OddParity
}

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

case object Pos extends ParityMultiplier
case object Neg extends ParityMultiplier
