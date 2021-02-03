package org.bitcoins.core.p2p

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.p2p._
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits._

/** Encapsulated network IP address currently uses the following structure
  * @see https://bitcoin.org/en/developer-reference#addr
  */
sealed abstract class NetworkIpAddress extends NetworkElement {

  /** Added in protocol version 31402.
    * A time in Unix epoch time format. Nodes advertising their own IP address set this to the current time.
    * Nodes advertising IP addresses they’ve connected to set this to the last time they connected to that node.
    * Other nodes just relaying the IP address should not change the time.
    * Nodes can use the time field to avoid relaying old addr messages.
    * Malicious nodes may change times or even set them in the future.
    */
  def time: UInt32

  /** The services the node advertised in its version message.
    */
  def services: ServiceIdentifier

  /** IPv6 address in big endian byte order.
    * IPv4 addresses can be provided as IPv4-mapped IPv6 addresses
    */
  def address: InetAddress

  /** Port number in big endian byte order.
    * Note that Bitcoin Core will only connect to nodes with non-standard port numbers as
    * a last resort for finding peers. This is to prevent anyone from trying to use the
    * network to disrupt non-Bitcoin services that run on other ports.
    */
  def port: Int

  override def bytes: ByteVector = RawNetworkIpAddressSerializer.write(this)
}

object NetworkIpAddress extends Factory[NetworkIpAddress] {

  /** Writes an IP address to the representation that the p2p network requires.
    * An IPv6 address is in big endian byte order.
    * An IPv4 address has to be mapped to an IPv6 address.
    *
    * @see https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
    */
  def writeAddress(iNetAddress: InetAddress): ByteVector = {
    if (iNetAddress.getAddress.size == 4) {
      //this means we need to convert the IPv4 address to an IPv6 address
      //first we have an 80 bit prefix of zeros
      val zeroBytes = ByteVector.fill(10)(0)
      //the next 16 bits are ones
      val oneBytes = hex"ffff"

      val prefix: ByteVector = zeroBytes ++ oneBytes
      val addr = prefix ++ ByteVector(iNetAddress.getAddress)
      addr
    } else {
      ByteVector(iNetAddress.getAddress)
    }
  }

  private case class NetworkIpAddressImpl(
      time: UInt32,
      services: ServiceIdentifier,
      address: InetAddress,
      port: Int)
      extends NetworkIpAddress

  def apply(
      time: UInt32,
      services: ServiceIdentifier,
      address: InetAddress,
      port: Int): NetworkIpAddress = {
    NetworkIpAddressImpl(time, services, address, port)
  }

  def fromBytes(bytes: ByteVector): NetworkIpAddress =
    RawNetworkIpAddressSerializer.read(bytes)

  def fromInetSocketAddress(
      socket: InetSocketAddress,
      services: ServiceIdentifier): NetworkIpAddress = {
    //TODO: this might be wrong, read this time documentation above
    val timestamp = UInt32(System.currentTimeMillis() / 1000)

    NetworkIpAddress(
      time = timestamp,
      services = services,
      address = socket.getAddress,
      port = socket.getPort
    )
  }
}
