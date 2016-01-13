package org.scalacoin.protocol

/**
 * Created by chrids s on 1/12/16.
 */
sealed trait AddressType

case object P2PKH extends AddressType {
  override def toString = "pubkeyhash"
}

case object P2SH extends AddressType {
  override def toString = "scripthash"
}

case object NonStandard extends AddressType {
  override def toString = "non-standard"
}

