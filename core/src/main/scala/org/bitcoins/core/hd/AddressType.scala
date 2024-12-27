package org.bitcoins.core.hd

import org.bitcoins.crypto.StringFactory

/** The address types covered by BIP44, BIP49 and BIP84 */
sealed abstract class AddressType {
  def altName: String
}

object AddressType extends StringFactory[AddressType] {

  /** Uses BIP84 address derivation, gives bech32 address (`bc1...`) */
  case object SegWit extends AddressType {
    override def altName: String = "bech32"
  }

  /** Uses BIP49 address derivation, gives SegWit addresses wrapped in P2SH
    * addresses (`3...`)
    */
  case object NestedSegWit extends AddressType {
    override def altName: String = "p2sh-segwit"
  }

  /** Uses BIP44 address derivation (`1...`) */
  case object Legacy extends AddressType {
    override def altName: String = "legacy"
  }

  case object P2TR extends AddressType {
    override def altName: String = "p2tr"
  }

  private val all = Vector(SegWit, NestedSegWit, Legacy, P2TR)

  override def fromStringOpt(str: String): Option[AddressType] = {
    all.find(_.toString.toLowerCase == str.toLowerCase) match {
      case Some(addressType) => Some(addressType)
      case None =>
        all.find(_.altName.toLowerCase == str.toLowerCase)
    }
  }

  override def fromString(string: String): AddressType = {
    fromStringOpt(string) match {
      case Some(addressType) => addressType
      case None =>
        sys.error(s"Could not find address type for string=$string")
    }
  }

  def fromPurpose(purpose: HDPurpose): Option[AddressType] = {
    purpose match {
      case HDPurpose.Legacy       => Some(Legacy)
      case HDPurpose.NestedSegWit => Some(NestedSegWit)
      case HDPurpose.SegWit       => Some(SegWit)
      case HDPurpose.Taproot      => Some(P2TR)
      case _: HDPurpose           => None
    }
  }
}
