package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.crypto.StringFactory

sealed abstract class SigningVersion {
  def nonceTag: String
  def announcementTag: String
  def outcomeTag: String
}

object SigningVersion extends StringFactory[SigningVersion] {

  /** Initial signing version that was created, not a part of any spec */
  final case object Mock extends SigningVersion {
    override def nonceTag: String = "DLCv0/Nonce"

    override def announcementTag: String = "DLCv0/Announcement"

    override def outcomeTag: String = "DLCv0/Outcome"
  }

  val latest: SigningVersion = Mock

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
