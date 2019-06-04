package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.TypeIdentifier
import org.bitcoins.node.messages.data.Inventory
import org.bitcoins.node.messages.TypeIdentifier
import org.bitcoins.node.messages.data.Inventory
import scodec.bits.ByteVector

/**
  * Created by chris on 6/1/16.
  * Serializes/deserializes a inventory
  * https://bitcoin.org/en/developer-reference#term-inventory
  */
trait RawInventorySerializer extends RawBitcoinSerializer[Inventory] {

  override def read(bytes: ByteVector): Inventory = {
    val typeIdentifier = TypeIdentifier(bytes.take(4))
    val hash = DoubleSha256Digest(bytes.slice(4, bytes.size))
    Inventory(typeIdentifier, hash)
  }

  override def write(inventory: Inventory): ByteVector = {
    inventory.typeIdentifier.bytes ++ inventory.hash.bytes
  }
}

object RawInventorySerializer extends RawInventorySerializer
