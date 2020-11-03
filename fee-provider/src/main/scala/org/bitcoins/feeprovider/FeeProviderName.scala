package org.bitcoins.feeprovider

import org.bitcoins.crypto.StringFactory

sealed abstract class FeeProviderName

object FeeProviderName extends StringFactory[FeeProviderName] {

  final case object BitcoinerLive extends FeeProviderName

  final case object BitGo extends FeeProviderName

  final case object Constant extends FeeProviderName

  final case object MempoolSpace extends FeeProviderName

  val all: Vector[FeeProviderName] =
    Vector(BitcoinerLive, BitGo, Constant, MempoolSpace)

  override def fromStringOpt(str: String): Option[FeeProviderName] = {
    all.find(_.toString.toLowerCase == str.toLowerCase)
  }

  override def fromString(string: String): FeeProviderName = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find FeeProviderName for string=$string")
    }
  }
}
