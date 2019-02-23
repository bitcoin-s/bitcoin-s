package org.bitcoins.node.messages.control

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.{Int32, Int64, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.Factory
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.VersionMessage
import org.bitcoins.node.serializers.messages.control.RawVersionMessageSerializer
import org.bitcoins.node.versions.ProtocolVersion
import org.joda.time.DateTime
import scodec.bits.ByteVector

/**
  * Created by chris on 6/3/16.
  * Companion object responsible for creating VersionMessages on the p2p network
  * https://bitcoin.org/en/developer-reference#version
  */
object VersionMessage extends Factory[VersionMessage] {

  private case class VersionMessageImpl(
      version: ProtocolVersion,
      services: ServiceIdentifier,
      timestamp: Int64,
      addressReceiveServices: ServiceIdentifier,
      addressReceiveIpAddress: InetAddress,
      addressReceivePort: Int,
      addressTransServices: ServiceIdentifier,
      addressTransIpAddress: InetAddress,
      addressTransPort: Int,
      nonce: UInt64,
      userAgentSize: CompactSizeUInt,
      userAgent: String,
      startHeight: Int32,
      relay: Boolean)
      extends VersionMessage

  override def fromBytes(bytes: ByteVector): VersionMessage =
    RawVersionMessageSerializer.read(bytes)

  def apply(
      version: ProtocolVersion,
      services: ServiceIdentifier,
      timestamp: Int64,
      addressReceiveServices: ServiceIdentifier,
      addressReceiveIpAddress: InetAddress,
      addressReceivePort: Int,
      addressTransServices: ServiceIdentifier,
      addressTransIpAddress: InetAddress,
      addressTransPort: Int,
      nonce: UInt64,
      userAgent: String,
      startHeight: Int32,
      relay: Boolean): VersionMessage = {
    val userAgentSize: CompactSizeUInt =
      CompactSizeUInt.calculateCompactSizeUInt(ByteVector(userAgent.getBytes))
    VersionMessageImpl(
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
      userAgentSize = userAgentSize,
      userAgent = userAgent,
      startHeight = startHeight,
      relay = relay
    )
  }

  def apply(
      network: NetworkParameters,
      receivingIpAddress: InetAddress): VersionMessage = {
    val transmittingIpAddress = InetAddress.getLocalHost
    VersionMessage(network, receivingIpAddress, transmittingIpAddress)
  }

  def apply(
      network: NetworkParameters,
      receivingIpAddress: InetAddress,
      transmittingIpAddress: InetAddress): VersionMessage = {
    val nonce = UInt64.zero
    val userAgent = Constants.userAgent
    val startHeight = Int32.zero
    val relay = false
    VersionMessage(
      version = Constants.version,
      services = UnnamedService,
      timestamp = Int64(DateTime.now.getMillis),
      addressReceiveServices = UnnamedService,
      addressReceiveIpAddress = receivingIpAddress,
      addressReceivePort = network.port,
      addressTransServices = NodeNetwork,
      addressTransIpAddress = transmittingIpAddress,
      addressTransPort = network.port,
      nonce = nonce,
      userAgent = userAgent,
      startHeight = startHeight,
      relay = relay
    )
  }

  def apply(host: String, network: NetworkParameters): VersionMessage = {
    //network.dnsSeeds(0)
    val transmittingIpAddress = InetAddress.getByName(host)
    VersionMessage(network, transmittingIpAddress)
  }

  def apply(
      socket: InetSocketAddress,
      network: NetworkParameters): VersionMessage = {
    VersionMessage(network, socket.getAddress)
  }
}
