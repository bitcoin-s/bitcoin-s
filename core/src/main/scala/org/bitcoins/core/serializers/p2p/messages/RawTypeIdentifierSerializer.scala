package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.TypeIdentifier
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/**
  * Created by chris on 5/31/16.
  * Reads and writes a type identifier on a peer-to-peer network
  * https://bitcoin.org/en/developer-reference#data-messages
  */
trait RawTypeIdentifierSerializer extends RawBitcoinSerializer[TypeIdentifier] {

  override def read(bytes: ByteVector): TypeIdentifier = {
    TypeIdentifier(UInt32(bytes.reverse))
  }

  override def write(typeIdentifier: TypeIdentifier): ByteVector = {
    typeIdentifier.num.bytes.reverse
  }
}
object RawTypeIdentifierSerializer extends RawTypeIdentifierSerializer
