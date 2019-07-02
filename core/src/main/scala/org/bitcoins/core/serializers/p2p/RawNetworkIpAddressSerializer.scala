package org.bitcoins.core.serializers.p2p

import java.net.InetAddress

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p._
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger
import scodec.bits.ByteVector

/**
  * Responsible for serializing and deserializing network ip address objects on the p2p network
  * @see https://bitcoin.org/en/developer-reference#addr
  */
trait RawNetworkIpAddressSerializer
    extends RawBitcoinSerializer[NetworkIpAddress]
    with BitcoinSLogger {

  def read(bytes: ByteVector): NetworkIpAddress = {
    val time = UInt32(bytes.take(4).reverse)
    val services = ServiceIdentifier(bytes.slice(4, 12))
    val ipBytes = bytes.slice(12, 28)
    val ipAddress = InetAddress.getByAddress(ipBytes.toArray)
    val port = bytes.slice(28, 30).toInt(signed = false)
    NetworkIpAddress(time, services, ipAddress, port)
  }

  def write(networkIpAddress: NetworkIpAddress): ByteVector = {
    val time = networkIpAddress.time.bytes.reverse
    val services = networkIpAddress.services.bytes
    val ipAddress = NetworkIpAddress.writeAddress(networkIpAddress.address)
    // uint16s are only 4 hex characters
    // cannot do fromShort,
    val port = ByteVector.fromInt(networkIpAddress.port, size = 2)
    time ++ services ++ ipAddress ++ port
  }

}

object RawNetworkIpAddressSerializer extends RawNetworkIpAddressSerializer
