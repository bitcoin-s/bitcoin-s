package org.bitcoins.node.serializers.messages.control

import java.net.InetAddress

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.{BitcoinSLogger, NumberUtil}
import org.bitcoins.node.messages.control.ServiceIdentifier
import org.bitcoins.node.util.{BitcoinSpvNodeUtil, NetworkIpAddress}
import org.bitcoins.node.util.{BitcoinSpvNodeUtil, NetworkIpAddress}
import scodec.bits.ByteVector

/**
  * Created by chris on 6/2/16.
  * Responsible for serializing and deserializing network ip address objects on the p2p network
  * https://bitcoin.org/en/developer-reference#addr
  */
trait RawNetworkIpAddressSerializer
    extends RawBitcoinSerializer[NetworkIpAddress]
    with BitcoinSLogger {

  def read(bytes: ByteVector): NetworkIpAddress = {
    val time = UInt32(bytes.take(4).reverse)
    val services = ServiceIdentifier(bytes.slice(4, 12))
    val ipBytes = bytes.slice(12, 28)
    val ipAddress = InetAddress.getByAddress(ipBytes.toArray)
    val port = NumberUtil.toLong(bytes.slice(28, 30)).toInt
    NetworkIpAddress(time, services, ipAddress, port)
  }

  def write(networkIpAddress: NetworkIpAddress): ByteVector = {
    val time = networkIpAddress.time.bytes.reverse
    val services = networkIpAddress.services.bytes
    val ipAddress = BitcoinSpvNodeUtil.writeAddress(networkIpAddress.address)
    //uint16s are only 4 hex characters
    val port = ByteVector.fromShort(networkIpAddress.port.toShort)
    time ++ services ++ ipAddress ++ port
  }

}

object RawNetworkIpAddressSerializer extends RawNetworkIpAddressSerializer
