package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import org.bitcoins.core.p2p.{Inventory, InventoryMessage}
import scodec.bits.ByteVector

import scala.annotation.tailrec

/**
  * Serializes and deserializes inventory objects on the peer-to-peer network
  * @see https://bitcoin.org/en/developer-reference#inv
  */
trait RawInventoryMessageSerializer
    extends RawBitcoinSerializer[InventoryMessage] {

  /**
    * Transforms a sequence of bytes into a Inventory object
    */
  override def read(bytes: ByteVector): InventoryMessage = {
    val inventoryCount = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val inventoryStart = inventoryCount.size.toInt
    val remainingBytes = bytes.slice(inventoryStart, bytes.size)
    val (inventories, _) = parseInventories(remainingBytes, inventoryCount)
    InventoryMessage(inventoryCount, inventories)
  }

  /**
    * Tranforms an inventory object into a hexadecimal string
    */
  override def write(inventoryMessage: InventoryMessage): ByteVector = {
    val msgBytes =
      RawSerializerHelper.writeNetworkElements(inventoryMessage.inventories)
    inventoryMessage.inventoryCount.bytes ++ msgBytes
  }

  /**
    * Parses the sequence of bytes into a sequence of inventories inside of the inventory message
    * @param bytes the bytes that need to be parsed into Inventories
    * @param requiredInventories the num of inventories inside this sequence of bytes
    * @return the sequence of inventories and the remaining bytes
    */
  private def parseInventories(
      bytes: ByteVector,
      requiredInventories: CompactSizeUInt): (List[Inventory], ByteVector) = {
    @tailrec
    def loop(
        remainingInventories: Long,
        remainingBytes: ByteVector,
        accum: List[Inventory]): (List[Inventory], ByteVector) = {
      if (remainingInventories <= 0) (accum.reverse, remainingBytes)
      else {
        val inventory = RawInventorySerializer.read(remainingBytes.slice(0, 36))
        loop(remainingInventories - 1,
             remainingBytes.slice(36, remainingBytes.size),
             inventory :: accum)
      }
    }
    loop(requiredInventories.num.toInt, bytes, List.empty)
  }
}

object RawInventoryMessageSerializer extends RawInventoryMessageSerializer
