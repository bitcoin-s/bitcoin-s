package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/**
  * Responsible for serializing and deserializing the
  * service identifier in a network message
  * @see https://bitcoin.org/en/developer-reference#version
  */
trait RawServiceIdentifierSerializer
    extends RawBitcoinSerializer[ServiceIdentifier] {

  override def read(bytes: ByteVector): ServiceIdentifier = {
    val serviceBytes = bytes.take(8)
    //since bitcoin uses big endian for numbers, we need to convert to little endian
    ServiceIdentifier(UInt64(serviceBytes.reverse))
  }

  override def write(serviceIdentifier: ServiceIdentifier): ByteVector = {
    serviceIdentifier.num.bytes.reverse
  }

}

object RawServiceIdentifierSerializer extends RawServiceIdentifierSerializer
