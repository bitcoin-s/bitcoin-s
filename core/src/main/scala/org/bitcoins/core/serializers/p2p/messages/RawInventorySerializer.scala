package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.{Inventory, TypeIdentifier}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

/** Serializes/deserializes a inventory
  * @see https://bitcoin.org/en/developer-reference#term-inventory
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
