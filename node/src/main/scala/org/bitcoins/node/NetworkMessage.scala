package org.bitcoins.node

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import org.bitcoins.node.headers.NetworkHeader
import org.bitcoins.node.messages.NetworkPayload
import org.bitcoins.node.serializers.RawNetworkMessageSerializer
import org.bitcoins.node.headers.NetworkHeader
import org.bitcoins.node.messages.NetworkPayload
import org.bitcoins.node.serializers.RawNetworkMessageSerializer
import scodec.bits.ByteVector

/**
  * Created by chris on 6/10/16.
  * Represents an entire p2p network message in bitcoins
  */
sealed abstract class NetworkMessage extends NetworkElement {
  def header: NetworkHeader
  def payload: NetworkPayload
  override def bytes: ByteVector = RawNetworkMessageSerializer.write(this)
}

object NetworkMessage extends Factory[NetworkMessage] {
  private case class NetworkMessageImpl(
      header: NetworkHeader,
      payload: NetworkPayload)
      extends NetworkMessage

  def fromBytes(bytes: ByteVector): NetworkMessage =
    RawNetworkMessageSerializer.read(bytes)

  /**
    * Creates a network message from it's [[NetworkHeader]] and [[NetworkPayload]]
    * @param header the [[NetworkHeader]] which is being sent across the network
    * @param payload the [[NetworkPayload]] which contains the information being sent across the network
    * @return
    */
  def apply(header: NetworkHeader, payload: NetworkPayload): NetworkMessage = {
    NetworkMessageImpl(header, payload)
  }

  /**
    * Creates a [[NetworkMessage]] out of it's [[NetworkPayload]]
    * @param network the [[NetworkParameters]] indicating the network which the message is going to be sent on
    * @param payload the payload that needs to be sent across the network
    * @return
    */
  def apply(
      network: NetworkParameters,
      payload: NetworkPayload): NetworkMessage = {
    val header = NetworkHeader(network, payload)
    NetworkMessage(header, payload)
  }
}
