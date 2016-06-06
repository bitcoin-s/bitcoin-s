package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
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
    val versionHex = BitcoinSUtil.encodeHex(versionBytes)
    val version = BitcoinSUtil.hexToLong(versionHex)
    //previous header hash next 32 bytes
    val prevBlockHashBytes = bytes.slice(4, 36)
    val prevBlockHash : DoubleSha256Digest = DoubleSha256Digest(prevBlockHashBytes)
    //merkle hash next 32 bytes
    val merkleRootBytes = bytes.slice(36, 68)
    val merkleRoot : DoubleSha256Digest = DoubleSha256Digest(merkleRootBytes)
    //time 4 bytes
    val timeBytes = bytes.slice(68,72)
    val timeHex = BitcoinSUtil.encodeHex(timeBytes)
    val time = BitcoinSUtil.hexToLong(timeHex)
    //nbits 4 bytes
    val nBitsBytes = bytes.slice(72,76)
    val nBitsHex = BitcoinSUtil.encodeHex(nBitsBytes)
    val nBits = BitcoinSUtil.hexToLong(nBitsHex)
    //nonce 4 bytes
    val nonceBytes = bytes.slice(76,80)
    val nonceHex = BitcoinSUtil.encodeHex(nonceBytes)
    val nonce = BitcoinSUtil.hexToLong(nonceHex)
    BlockHeader(version,prevBlockHash, merkleRoot, time, nBits, nonce)
  }

  /**
    * Serializes the BlockHeader to a hexadecimal string
    *
    * @param blockHeader the block header to be serialized
    * @return the hexadecimal string representing the block header
    */
  def write(blockHeader: BlockHeader) : String = {
    val headerVersion = BitcoinSUtil.flipEndianess(blockHeader.version.toHexString)
    val versionSubPadding = addPrecedingZero(headerVersion)
    val version = addPadding(8,versionSubPadding)

    val prevHash = blockHeader.previousBlockHash.hex
    val merkleRoot = blockHeader.merkleRootHash.hex

    val time = BitcoinSUtil.flipEndianess(blockHeader.time.toHexString)
    val nBits = BitcoinSUtil.flipEndianess(blockHeader.nBits.toHexString)
    val nonce = BitcoinSUtil.flipEndianess(blockHeader.nonce.toHexString)

    version + prevHash + merkleRoot + time + nBits + nonce
  }

}

object RawBlockHeaderSerializer extends RawBlockHeaderSerializer