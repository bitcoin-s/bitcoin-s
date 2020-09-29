package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.crypto.StringFactory

sealed abstract class SigningVersion

object SigningVersion extends StringFactory[SigningVersion] {

  /** Initial signing version that was created with krystal bull, not a part of any spec */
  final case object Mock extends SigningVersion

  val all: Vector[SigningVersion] = Vector(Mock)

  override def fromStringOpt(str: String): Option[SigningVersion] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): SigningVersion = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find signing version for string=${string}")
    }
  }
}
