package org.bitcoins.core.p2p

import scodec.bits._

trait InetAddress {
  def bytes: ByteVector

  def ipv4Bytes: ByteVector = {
    require(bytes.take(12) == hex"00000000000000000000ffff",
            "Cannot call ipv4Bytes for an IPv6 address")
    bytes.drop(12)
  }

  def getAddress: Array[Byte] = bytes.toArray
}

object InetAddress {

  private case class InetAddressImpl(
      bytes: ByteVector
  ) extends InetAddress

  def apply(array: Array[Byte]): InetAddress = {
    getByAddress(array)
  }

  def apply(bytes: ByteVector): InetAddress = {
    getByAddress(bytes.toArray)
  }

  def getByAddress(array: Array[Byte]): InetAddress = {
    val bytes = NetworkIpAddress.writeAddress(ByteVector(array))
    InetAddressImpl(bytes)
  }
}

trait TorAddress extends InetAddress {

  override def ipv4Bytes: ByteVector = throw new IllegalArgumentException(
    "Tor address cannot be an IPv4 address")
}

object TorAddress {
  val TOR_V2_ADDR_LENGTH = 10
  val TOR_V3_ADDR_LENGTH = 32
}

trait Tor2Address extends TorAddress

object Tor2Address {

  private case class Tor2AddressImpl(bytes: ByteVector) extends Tor2Address {
    require(bytes.size == TorAddress.TOR_V2_ADDR_LENGTH)
  }
}

trait Tor3Address extends TorAddress

object Tor3Address {

  private case class Tor3AddressImpl(bytes: ByteVector) extends Tor2Address {
    require(bytes.size == TorAddress.TOR_V3_ADDR_LENGTH)
  }
}
