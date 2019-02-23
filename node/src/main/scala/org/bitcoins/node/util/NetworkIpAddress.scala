package org.bitcoins.node.util

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.control.{NodeNetwork, ServiceIdentifier}
import org.bitcoins.node.messages.control.ServiceIdentifier
import org.bitcoins.node.serializers.messages.control.RawNetworkIpAddressSerializer
import scodec.bits.ByteVector

/**
  * Created by chris on 5/31/16.
  * Encapsulated network IP address currently uses the following structure
  * https://bitcoin.org/en/developer-reference#addr
  */
sealed abstract class NetworkIpAddress extends NetworkElement {

  /**
    * Added in protocol version 31402.
    * A time in Unix epoch time format. Nodes advertising their own IP address set this to the current time.
    * Nodes advertising IP addresses they’ve connected to set this to the last time they connected to that node.
    * Other nodes just relaying the IP address should not change the time.
    * Nodes can use the time field to avoid relaying old addr messages.
    * Malicious nodes may change times or even set them in the future.
    *
    * @return
    */
  def time: UInt32

  /**
    * The services the node advertised in its version message.
    *
    * @return
    */
  def services: ServiceIdentifier

  /**
    * IPv6 address in big endian byte order.
    * IPv4 addresses can be provided as IPv4-mapped IPv6 addresses
    *
    * @return
    */
  def address: InetAddress

  /**
    * Port number in big endian byte order.
    * Note that Bitcoin Core will only connect to nodes with non-standard port numbers as
    * a last resort for finding peers. This is to prevent anyone from trying to use the
    * network to disrupt non-Bitcoin services that run on other ports.
    *
    * @return
    */
  def port: Int

  override def bytes: ByteVector = RawNetworkIpAddressSerializer.write(this)
}

object NetworkIpAddress extends Factory[NetworkIpAddress] {
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

  def fromInetSocketAddress(socket: InetSocketAddress): NetworkIpAddress = {
    //TODO: this might be wrong, read this time documentation above
    val timestamp = UInt32(System.currentTimeMillis() / 1000)

    NetworkIpAddress(
      time = timestamp,
      services = NodeNetwork,
      address = socket.getAddress,
      port = socket.getPort
    )
  }
}
