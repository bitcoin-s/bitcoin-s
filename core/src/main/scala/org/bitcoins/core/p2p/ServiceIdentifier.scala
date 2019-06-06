package org.bitcoins.core.p2p

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.p2p.messages.RawServiceIdentifierSerializer
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

/**
  * Indicates the services that are provided by this spv node
  * @see [[https://bitcoin.org/en/developer-reference#version]]
  */
sealed trait ServiceIdentifier extends NetworkElement {
  def num: UInt64
  override def bytes: ByteVector = RawServiceIdentifierSerializer.write(this)
}

/**
  * This node is not a full node.
  * It may not be able to provide any data except for the transactions it originates.
  */
case object UnnamedService extends ServiceIdentifier {
  override val num = UInt64.zero
}

/**
  * This is a full node and can be asked for full blocks.
  * It should implement all protocol features available in its self-reported protocol version.
  */
case object NodeNetwork extends ServiceIdentifier {
  override val num = UInt64.one
}

/**
  * Designated type for any service that does not have value of 0 or 1
  */
sealed trait UnknownService extends ServiceIdentifier

object ServiceIdentifier extends Factory[ServiceIdentifier] {

  private case class UnknownServiceImpl(num: UInt64) extends UnknownService

  def fromBytes(bytes: ByteVector): ServiceIdentifier =
    RawServiceIdentifierSerializer.read(bytes)

  def apply(num: BigInt): ServiceIdentifier = ServiceIdentifier(UInt64(num))

  def apply(uInt64: UInt64): ServiceIdentifier = uInt64 match {
    case UInt64.zero => UnnamedService
    case UInt64.one  => NodeNetwork
    case x: UInt64   => UnknownServiceImpl(x)
  }
}
