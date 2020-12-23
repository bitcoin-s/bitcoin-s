package org.bitcoins.core.api.chain.db

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

case class BlockHeaderDb(
    height: Int,
    hashBE: DoubleSha256DigestBE,
    version: Int32,
    previousBlockHashBE: DoubleSha256DigestBE,
    merkleRootHashBE: DoubleSha256DigestBE,
    time: UInt32,
    nBits: UInt32,
    nonce: UInt32,
    hex: String,
    chainWork: BigInt) {

  lazy val blockHeader: BlockHeader = {
    val blockHeader = BlockHeader.fromHex(hex)

    require(blockHeader.hashBE == hashBE)
    require(blockHeader.previousBlockHashBE == previousBlockHashBE)
    require(blockHeader.version == version)
    require(blockHeader.nBits == nBits)
    require(blockHeader.nonce == nonce)

    blockHeader
  }

  lazy val difficulty: BigInt = blockHeader.difficulty

  lazy val hash: DoubleSha256Digest = hashBE.flip

  override def toString: String = {
    s"BlockHeaderDb(height=$height,hashBE=${hashBE.hex},version=$version," +
      s"prevBlockHashBE=${previousBlockHashBE.hex}" +
      s"merkleRootHashBE=${merkleRootHashBE.hex}," +
      s"time=$time,nBits=$nBits,nonce=$nonce,chainWork=$chainWork)"
  }
}

object BlockHeaderDbHelper {

  def fromBlockHeader(
      height: Int,
      chainWork: BigInt,
      bh: BlockHeader): BlockHeaderDb = {
    BlockHeaderDb(
      height = height,
      hashBE = bh.hashBE,
      previousBlockHashBE = bh.previousBlockHashBE,
      merkleRootHashBE = bh.merkleRootHashBE,
      time = bh.time,
      nBits = bh.nBits,
      nonce = bh.nonce,
      version = bh.version,
      hex = bh.hex,
      chainWork = chainWork
    )
  }
}
