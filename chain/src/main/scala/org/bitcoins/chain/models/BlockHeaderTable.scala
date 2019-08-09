package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import slick.jdbc.SQLiteProfile.api._

case class BlockHeaderDb(
    height: Int,
    hashBE: DoubleSha256DigestBE,
    version: Int32,
    previousBlockHashBE: DoubleSha256DigestBE,
    merkleRootHashBE: DoubleSha256DigestBE,
    time: UInt32,
    nBits: UInt32,
    nonce: UInt32,
    hex: String) {

  lazy val blockHeader: BlockHeader = {
    val blockHeader = BlockHeader.fromHex(hex)

    require(blockHeader.hashBE == hashBE)
    require(blockHeader.previousBlockHashBE == previousBlockHashBE)
    require(blockHeader.version == version)
    require(blockHeader.nBits == nBits)
    require(blockHeader.nonce == nonce)

    blockHeader
  }
}

object BlockHeaderDbHelper {

  def fromBlockHeader(height: Int, bh: BlockHeader): BlockHeaderDb = {
    BlockHeaderDb(
      height = height,
      hashBE = bh.hashBE,
      previousBlockHashBE = bh.previousBlockHashBE,
      merkleRootHashBE = bh.merkleRootHashBE,
      time = bh.time,
      nBits = bh.nBits,
      nonce = bh.nonce,
      version = bh.version,
      hex = bh.hex
    )
  }
}

/** A table that stores block headers related to a blockchain */
class BlockHeaderTable(tag: Tag)
    extends Table[BlockHeaderDb](tag, "block_headers") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def height = column[Int]("height")

  def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

  def version = column[Int32]("version")

  def previousBlockHash = column[DoubleSha256DigestBE]("previous_block_hash")

  def merkleRootHash = column[DoubleSha256DigestBE]("merkle_root_hash")

  def time = column[UInt32]("time")

  def nBits = column[UInt32]("n_bits")

  def nonce = column[UInt32]("nonce")

  def hex = column[String]("hex")

  /** The sql index for searching based on [[height]] */
  def heightIndex = index("height_index", height)

  def hashIndex = index("hash_index", hash)

  def * = {
    (height,
     hash,
     version,
     previousBlockHash,
     merkleRootHash,
     time,
     nBits,
     nonce,
     hex).<>(BlockHeaderDb.tupled, BlockHeaderDb.unapply)
  }

}
