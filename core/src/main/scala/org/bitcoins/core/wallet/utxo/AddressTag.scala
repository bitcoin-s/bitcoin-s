package org.bitcoins.core.wallet.utxo

import org.bitcoins.crypto.StringFactory

/** A type of address tag, many AddressTags of the same type
  * should inherit the AddressTagType that they all share
  */
trait AddressTagType {
  def typeName: String

  def ==(at: AddressTagType): Boolean = typeName == at.typeName
  def !=(at: AddressTagType): Boolean = !(this == at)
}

trait AddressTagName {
  def name: String

  def ==(at: AddressTagType): Boolean = name == at.typeName
  def !=(at: AddressTagType): Boolean = !(this == at)
}

/** An tag for an address. It's name is what it is referred to as
  * and it's tagType is its parent AddressTagType
  */
trait AddressTag {
  def tagName: AddressTagName
  def tagType: AddressTagType

  def ==(at: AddressTag): Boolean =
    tagName == at.tagName && tagType == at.tagType
  def !=(at: AddressTag): Boolean = !(this == at)
}

trait AddressTagFactory[Tag <: AddressTag] extends StringFactory[Tag] {

  def tagType: AddressTagType

  def tagNames: Vector[AddressTagName]

  def all: Vector[Tag]

  override def fromStringOpt(str: String): Option[Tag] =
    all.find(tag => str.toLowerCase() == tag.toString.toLowerCase)

  override def fromString(string: String): Tag = {
    fromStringOpt(string) match {
      case Some(t) => t
      case None    => sys.error(s"Could not find tag for string=${string}")
    }
  }
}
