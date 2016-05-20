package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.serializers.RawBitcoinSerializer

/**
  * Created by chris on 5/19/16.
  * Serializes block headers
  * https://bitcoin.org/en/developer-reference#block-headers
  */
trait RawBlockHeaderSerializer extends RawBitcoinSerializer[BlockHeader] {

  /**
    * Converts a list of bytes into a block header
    * @param bytes the bytes to parsed into a block header
    * @return the block header
    */
  def read(bytes : List[Byte]) : BlockHeader = ???

  /**
    * Serializes the BlockHeader to a hexadecimal string
    * @param blockHeader the block header to be serialized
    * @return the hexadecimal string representing the block header
    */
  def write(blockHeader: BlockHeader) : String = ???

}

object RawBlockHeaderSerializer extends RawBlockHeaderSerializer