package org.bitcoins.core.p2p

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import org.bitcoins.core.serializers.p2p.messages.RawInventorySerializer
import scodec.bits.ByteVector

/**
  * These are used as unique identifiers inside the peer-to-peer network
  * @see [[https://bitcoin.org/en/developer-reference#term-inventory]]
  */
trait Inventory extends NetworkElement {

  /**
    * The type of object which was hashed
    * @return
    */
  def typeIdentifier: TypeIdentifier

  /**
    * SHA256(SHA256()) hash of the object in internal byte order.
    * @return
    */
  def hash: DoubleSha256Digest

  override def bytes: ByteVector = RawInventorySerializer.write(this)
}

object Inventory extends Factory[Inventory] {

  private case class InventoryImpl(
      typeIdentifier: TypeIdentifier,
      hash: DoubleSha256Digest)
      extends Inventory

  override def fromBytes(bytes: ByteVector): Inventory =
    RawInventorySerializer.read(bytes)

  def apply(
      typeIdentifier: TypeIdentifier,
      hash: DoubleSha256Digest): Inventory = {
    InventoryImpl(typeIdentifier, hash)
  }
}



