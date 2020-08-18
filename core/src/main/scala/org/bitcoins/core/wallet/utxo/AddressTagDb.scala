package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.protocol.BitcoinAddress

case class AddressTagDb(
    address: BitcoinAddress,
    tagName: AddressTagName,
    tagType: AddressTagType) {

  val addressTag: AddressTag = InternalAddressTag(tagName, tagType)

  def ==(at: AddressTagDb): Boolean =
    address == at.address && addressTag == at.addressTag

  def !=(at: AddressTagDb): Boolean = !(this == at)
}

object AddressTagDb {

  def apply(address: BitcoinAddress, addressTag: AddressTag): AddressTagDb =
    AddressTagDb(address, addressTag.tagName, addressTag.tagType)
}
