package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.serializers.RawBitcoinSerializer

/**
  * Created by chris on 5/20/16.
  * Responsible for serializing blocks in our blockchain
  * https://bitcoin.org/en/developer-reference#serialized-blocks
  */
trait RawBlockSerializer extends RawBitcoinSerializer[Block] {

  /**
    * Takes a list of bytes and converts it into a Block
    * @param bytes the bytes to be converted to a block
    * @return the block object parsed from the list of bytes
    */
  def read(bytes : List[Byte]) : Block = ???

  /**
    * Takes in a block and converts it a hexadecimal string
    * @param block the block that needs to be converted to a hexadecimal string
    * @return the hexadecimal string representing a block
    */
  def write(block : Block) : String = ???

}


object RawBlockSerializer extends RawBlockSerializer