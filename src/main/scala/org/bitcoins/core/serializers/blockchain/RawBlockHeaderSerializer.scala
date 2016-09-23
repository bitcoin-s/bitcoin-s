package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.{CryptoUtil, BitcoinSUtil}

/**
  * Created by chris on 5/19/16.
  * Serializes block headers
  * https://bitcoin.org/en/developer-reference#block-headers
  */
trait RawBlockHeaderSerializer extends RawBitcoinSerializer[BlockHeader] {

  /**
    * Converts a list of bytes into a block header
    *
    * @param bytes the bytes to parsed into a block header
    * @return the block header
    */
  def read(bytes : List[Byte]) : BlockHeader = {
    //version first 4 bytes
    val versionBytes = bytes.slice(0,4)
    val versionHex = BitcoinSUtil.encodeHex(versionBytes.reverse)
    val version = UInt32(versionHex)
    //previous header hash next 32 bytes
    val prevBlockHashBytes = bytes.slice(4, 36)
    val prevBlockHash : DoubleSha256Digest = DoubleSha256Digest(prevBlockHashBytes)
    //merkle hash next 32 bytes
    val merkleRootBytes = bytes.slice(36, 68)
    val merkleRoot : DoubleSha256Digest = DoubleSha256Digest(merkleRootBytes)
    //time 4 bytes
    val timeBytes = bytes.slice(68,72)
    val timeHex = BitcoinSUtil.encodeHex(timeBytes.reverse)
    val time = UInt32(timeHex)
    //nbits 4 bytes
    val nBitsBytes = bytes.slice(72,76)
    val nBitsHex = BitcoinSUtil.encodeHex(nBitsBytes.reverse)
    val nBits = UInt32(nBitsHex)
    //nonce 4 bytes
    val nonceBytes = bytes.slice(76,80)
    val nonceHex = BitcoinSUtil.encodeHex(nonceBytes.reverse)
    val nonce = UInt32(nonceHex)
    BlockHeader(version,prevBlockHash, merkleRoot, time, nBits, nonce)
  }

  /**
    * Serializes the BlockHeader to a hexadecimal string
    *
    * @param blockHeader the block header to be serialized
    * @return the hexadecimal string representing the block header
    */
  def write(blockHeader: BlockHeader) : String = {
    //TODO: By happenstance, flipEndianness function adds a leading '0' if we have hex number less than 10.
    //We need to explicitly handle this case in our write function in the future.
    val headerVersion = BitcoinSUtil.flipEndianness(blockHeader.version.hex)
    val versionSubPadding = addPrecedingZero(headerVersion)
    val version = addPadding(8,versionSubPadding)

    val prevHash = blockHeader.previousBlockHash.hex
    val merkleRoot = blockHeader.merkleRootHash.hex

    val time = addPadding(8,BitcoinSUtil.flipEndianness(blockHeader.time.hex))
    val nBits = addPadding(8,BitcoinSUtil.flipEndianness(blockHeader.nBits.hex))
    val nonce = addPadding(8,BitcoinSUtil.flipEndianness(blockHeader.nonce.hex))

    version + prevHash + merkleRoot + time + nBits + nonce
  }

}

object RawBlockHeaderSerializer extends RawBlockHeaderSerializer