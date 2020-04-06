package org.bitcoins.core.wallet.utxo

trait AddressTag {
  val typeName: String
}

trait AddressTagFactory[TagType <: AddressTag] {

  val all: Vector[TagType]

  def fromString(str: String): Option[TagType] = {
    all.find(tag => str.toLowerCase() == tag.toString.toLowerCase)
  }
}
