package org.bitcoins.node.models

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import slick.jdbc.SQLiteProfile.api._

/**
  * Created by chris on 9/7/16.
  */
class BlockHeaderTable(tag: Tag)
    extends Table[BlockHeader](tag, "block_headers") {
  import ColumnMappers._

  def height = column[Long]("height")

  def hash = column[DoubleSha256Digest]("hash", O.PrimaryKey)

  def version = column[Int32]("version")

  def previousBlockHash = column[DoubleSha256Digest]("previous_block_hash")

  def merkleRootHash = column[DoubleSha256Digest]("merkle_root_hash")

  def time = column[UInt32]("time")

  def nBits = column[UInt32]("n_bits")

  def nonce = column[UInt32]("nonce")

  def hex = column[String]("hex")

  /** The sql index for searching based on [[height]] */
  def heightIndex = index("height_index", height)

  def * = {
    (height.?,
     hash,
     version,
     previousBlockHash,
     merkleRootHash,
     time,
     nBits,
     nonce,
     hex).<>(blockHeaderApply, blockHeaderUnapply)
  }

  /** Creates a block header from a tuple */
  private val blockHeaderApply: (
      (
          Option[Long],
          DoubleSha256Digest,
          Int32,
          DoubleSha256Digest,
          DoubleSha256Digest,
          UInt32,
          UInt32,
          UInt32,
          String)) => BlockHeader = {
    case (_: Option[Long],
          hash: DoubleSha256Digest,
          version: Int32,
          previousBlockHash: DoubleSha256Digest,
          merkleRootHash: DoubleSha256Digest,
          time: UInt32,
          nBits: UInt32,
          nonce: UInt32,
          hex: String) =>
      val header = BlockHeader(version,
                               previousBlockHash,
                               merkleRootHash,
                               time,
                               nBits,
                               nonce)
      require(
        header.hash == hash && header.hex == hex,
        "Block header is not giving us the same hash that was stored in the database, " +
          "got: " + header.hash + " expected: " + hash + "\nStored hex: " + hex + "actual hex: " + header.hex
      )
      header
  }

  /** Destructs a block header to a tuple */
  private val blockHeaderUnapply: BlockHeader => Option[
    (
        Option[Long],
        DoubleSha256Digest,
        Int32,
        DoubleSha256Digest,
        DoubleSha256Digest,
        UInt32,
        UInt32,
        UInt32,
        String)] = { blockHeader: BlockHeader =>
    Some(
      (None,
       blockHeader.hash,
       blockHeader.version,
       blockHeader.previousBlockHash,
       blockHeader.merkleRootHash,
       blockHeader.time,
       blockHeader.nBits,
       blockHeader.nonce,
       blockHeader.hex))
  }

}
