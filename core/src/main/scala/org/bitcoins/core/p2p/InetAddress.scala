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
