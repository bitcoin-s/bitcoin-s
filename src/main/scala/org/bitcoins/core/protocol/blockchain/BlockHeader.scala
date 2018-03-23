package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.blockchain.RawBlockHeaderSerializer
import org.bitcoins.core.util.{ BitcoinSUtil, CryptoUtil, BitcoinSLogger, Factory }

/**
 * Created by chris on 5/19/16.
 * Nodes collect new transactions into a block, hash them into a hash tree,
 * and scan through nonce values to make the block's hash satisfy proof-of-work
 * requirements.  When they solve the proof-of-work, they broadcast the block
 * to everyone and the block is added to the block chain.  The first transaction
 * in the block is a special one that creates a new coin owned by the creator
 * of the block.
 * Bitcoin Developer reference link
 * https://bitcoin.org/en/developer-reference#block-headers
 * Bitcoin Core implementation:
 * https://github.com/bitcoin/bitcoin/blob/master/src/primitives/block.h#L20
 */
sealed trait BlockHeader extends NetworkElement {

  /**
   * The block version number indicates which set of block validation rules to follow.
   * See the list of block versions below.
   * See BIP9 for more information on what version number signify
   * https://github.com/bitcoin/bips/blob/master/bip-0009.mediawiki
   *
   * @return the version number for this block
   */
  def version: UInt32

  /**
   * A SHA256(SHA256()) hash in internal byte order of the previous block’s header.
   * This ensures no previous block can be changed without also changing this block’s header.
   *
   * @return the previous block's hash
   */

  def previousBlockHash: DoubleSha256Digest

  /**
   * A SHA256(SHA256()) hash in internal byte order.
   * The merkle root is derived from the hashes of all transactions included in this block,
   * ensuring that none of those transactions can be modified without modifying the header.
   * https://bitcoin.org/en/developer-reference#merkle-trees
   *
   * @return the merkle root of the merkle tree
   */

  def merkleRootHash: DoubleSha256Digest

  /**
   * The block time is a Unix epoch time when the miner started hashing the header (according to the miner).
   * Must be greater than or equal to the median time of the previous 11 blocks.
   * Full nodes will not accept blocks with headers more than two hours in the future according to their clock.
   *
   * @return the time when the miner started solving the block
   */
  def time: UInt32

  /**
   * An encoded version of the target threshold this block’s header hash must be less than or equal to.
   * See the nBits format described below.
   * https://bitcoin.org/en/developer-reference#target-nbits
   *
   * @return
   */
  def nBits: UInt32

  /**
   * An arbitrary number miners change to modify the header hash in order to produce a hash below the target threshold.
   * If all 32-bit values are tested, the time can be updated or the coinbase
   * transaction can be changed and the merkle root updated.
   *
   * @return the nonce used to try and solve a block
   */
  def nonce: UInt32

  /** Returns the block's hash */
  def hash: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  override def bytes: Seq[Byte] = RawBlockHeaderSerializer.write(this)

}

/**
 * Companion object used for creating BlockHeaders
 */
object BlockHeader extends Factory[BlockHeader] {

  private sealed case class BlockHeaderImpl(version: UInt32, previousBlockHash: DoubleSha256Digest,
                                            merkleRootHash: DoubleSha256Digest, time: UInt32, nBits: UInt32, nonce: UInt32) extends BlockHeader

  def apply(version: UInt32, previousBlockHash: DoubleSha256Digest, merkleRootHash: DoubleSha256Digest,
            time: UInt32, nBits: UInt32, nonce: UInt32): BlockHeader = {
    BlockHeaderImpl(version, previousBlockHash, merkleRootHash, time, nBits, nonce)
  }

  def fromBytes(bytes: Seq[Byte]): BlockHeader = RawBlockHeaderSerializer.read(bytes)

}