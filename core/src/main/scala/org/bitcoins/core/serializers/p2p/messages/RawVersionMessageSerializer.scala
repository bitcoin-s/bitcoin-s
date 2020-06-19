package org.bitcoins.core.serializers.p2p.messages

import java.net.InetAddress

import org.bitcoins.core.number.{Int32, Int64, UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.p2p._
import scodec.bits.ByteVector

/**
  * Responsible for serialization and deserialization of VersionMessages on the p2p network
  * @see https://bitcoin.org/en/developer-reference#version
  */
trait RawVersionMessageSerializer
    extends RawBitcoinSerializer[VersionMessage]
    with BitcoinSLogger {

  def read(bytes: ByteVector): VersionMessage = {
    val version = ProtocolVersion(bytes.take(4))

    val services = ServiceIdentifier(bytes.slice(4, 12))

    val timestamp = Int64(bytes.slice(12, 20).reverse)

    val addressReceiveServices = ServiceIdentifier(bytes.slice(20, 28))

    val addressReceiveIpAddress =
      InetAddress.getByAddress(bytes.slice(28, 44).toArray)

    val addressReceivePort = UInt32(bytes.slice(44, 46)).toInt

    val addressTransServices = ServiceIdentifier(bytes.slice(46, 54))

    val addressTransIpAddress =
      InetAddress.getByAddress(bytes.slice(54, 70).toArray)

    val addressTransPort = UInt32(bytes.slice(70, 72)).toInt

    val nonce = UInt64(bytes.slice(72, 80))

    val userAgentSize =
      CompactSizeUInt.parseCompactSizeUInt(bytes.slice(80, bytes.size))

    val userAgentBytesStartIndex = 80 + userAgentSize.byteSize.toInt

    val userAgentBytes = bytes.slice(
      userAgentBytesStartIndex,
      userAgentBytesStartIndex + userAgentSize.num.toInt)

    val userAgent = userAgentBytes.toArray.map(_.toChar).mkString

    val startHeightStartIndex =
      userAgentBytesStartIndex + userAgentSize.num.toInt

    val startHeight = Int32(
      bytes.slice(startHeightStartIndex, startHeightStartIndex + 4).reverse)

    val relay = bytes(startHeightStartIndex + 4) != 0

    VersionMessage(
      version = version,
      services = services,
      timestamp = timestamp,
      addressReceiveServices = addressReceiveServices,
      addressReceiveIpAddress = addressReceiveIpAddress,
      addressReceivePort = addressReceivePort,
      addressTransServices = addressTransServices,
      addressTransIpAddress = addressTransIpAddress,
      addressTransPort = addressTransPort,
      nonce = nonce,
      userAgent = userAgent,
      startHeight = startHeight,
      relay = relay
    )
  }

  def write(versionMessage: VersionMessage): ByteVector = {
    versionMessage.version.bytes ++
      versionMessage.services.bytes ++
      versionMessage.timestamp.bytes.reverse ++
      versionMessage.addressReceiveServices.bytes ++
      NetworkIpAddress.writeAddress(versionMessage.addressReceiveIpAddress) ++
      //encode hex returns 8 characters, but we only need the last 4 since port number is a uint16
      //check for precision loss here?
      ByteVector.fromShort(versionMessage.addressReceivePort.toShort) ++
      versionMessage.addressTransServices.bytes ++
      NetworkIpAddress.writeAddress(versionMessage.addressTransIpAddress) ++
      //encode hex returns 8 characters, but we only need the last 4 since port number is a uint16
      //check for precision loss here?
      ByteVector.fromShort(versionMessage.addressTransPort.toShort) ++
      versionMessage.nonce.bytes ++
      versionMessage.userAgentSize.bytes ++
      ByteVector(versionMessage.userAgent.getBytes) ++
      versionMessage.startHeight.bytes.reverse ++
      (if (versionMessage.relay) ByteVector.fromByte(1.toByte)
       else ByteVector.fromByte(0.toByte))
  }

}

object RawVersionMessageSerializer extends RawVersionMessageSerializer
