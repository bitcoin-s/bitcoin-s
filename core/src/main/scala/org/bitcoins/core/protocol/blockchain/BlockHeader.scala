package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.serializers.blockchain.RawBlockHeaderSerializer
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Nodes collect new transactions into a block, hash them into a hash tree,
  * and scan through nonce values to make the block's hash satisfy proof-of-work
  * requirements.  When they solve the proof-of-work, they broadcast the block
  * to everyone and the block is added to the block chain.  The first transaction
  * in the block is a special one that creates a new coin owned by the creator
  * of the block.
  * @see Bitcoin Developer reference:
  * https://bitcoin.org/en/developer-reference#block-headers
  *
  * @see Bitcoin Core implementation:
  * https://github.com/bitcoin/bitcoin/blob/master/src/primitives/block.h#L20
  */
sealed trait BlockHeader extends NetworkElement {

  /** The block version number indicates which set of block validation rules to follow.
    * See the list of block versions below.
    *
    * @see BIP9 for more information on what version number signify
    * https://github.com/bitcoin/bips/blob/master/bip-0009.mediawiki
    *
    * @return the version number for this block
    */
  def version: Int32

  /** A SHA256(SHA256()) hash in internal byte order of the previous block’s header.
    * This ensures no previous block can be changed without also changing this block’s header.
    *
    * @return the previous block's hash
    */
  def previousBlockHash: DoubleSha256Digest

  /** Returns the big endian encoding of the previous block hash
    * This is useful for using rpc and block exporers, but is NOT used in the protocol itself
    *
    * @see see this Stack Exchange question for more:
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation]]
    */
  def previousBlockHashBE: DoubleSha256DigestBE = previousBlockHash.flip

  /** A `SHA256(SHA256())` hash in internal byte order.
    * The merkle root is derived from the hashes of all transactions included in this block,
    * ensuring that none of those transactions can be modified without modifying the header.
    *
    * @see https://bitcoin.org/en/developer-reference#merkle-trees
    *
    * @return the merkle root of the merkle tree
    */
  def merkleRootHash: DoubleSha256Digest

  /** Returns the merkle root hash in BIG ENDIAN format. This is not compatible with the bitcoin
    * protocol but it is useful for rpc clients and block explorers
    *
    * @see this link for more info
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation]]
    */
  def merkleRootHashBE: DoubleSha256DigestBE = merkleRootHash.flip

  /** The block time is a Unix epoch time when the miner started hashing the header (according to the miner).
    * Must be greater than or equal to the median time of the previous 11 blocks.
    * Full nodes will not accept blocks with headers more than two hours in the future according to their clock.
    *
    * @return the time when the miner started solving the block
    */
  def time: UInt32

  /** An encoded version of the target threshold this block’s header hash must be less than or equal to.
    *
    * @see See the nBits format described below.
    * https://bitcoin.org/en/developer-reference#target-nbits
    */
  def nBits: UInt32

  /** This is the decoded version of [[nBits]]. nBits is used to compactly represent the difficulty
    * target for the bitcoin network. This field is the expanded version that is the _actual_
    * requirement needed for the network. This is a 256 bit unsigned integer
    * See the bitcoin developer reference for more information on how this is constructed
    * [[https://bitcoin.org/en/developer-reference#target-nbits documentation]]
    *
    * The hash of this block needs to be _less than_ this difficulty
    * to be considered a valid block on the network
    */
  def difficulty: BigInt = {
    NumberUtil.targetExpansion(nBits = nBits).difficulty
  }

  /** An arbitrary number miners change to modify the header hash in order to produce a hash below the target threshold.
    * If all 32-bit values are tested, the time can be updated or the coinbase
    * transaction can be changed and the merkle root updated.
    *
    * @return the nonce used to try and solve a block
    */
  def nonce: UInt32

  /** Returns the block's hash in the protocol level little endian encoding */
  lazy val hash: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /** Returns the block hash in big endian format, this is useful for rpc
    * and block explorer debugging. This is *not* used in the core protocol itself.
    * See this link for more info
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation]]
    * @return
    */
  lazy val hashBE: DoubleSha256DigestBE = hash.flip

  override lazy val bytes: ByteVector = RawBlockHeaderSerializer.write(this)

  override def toString: String = {
    s"BlockHeader(hashBE=${hashBE.hex},version=$version," +
      s"prevBlockHashBE=${previousBlockHashBE.hex}," +
      s"merkleRootHashBE=${merkleRootHashBE.hex}," +
      s"time=$time,nBits=$nBits,nonce=$nonce)"
  }
}

/** Companion object used for creating BlockHeaders
  */
object BlockHeader extends Factory[BlockHeader] {

  sealed private case class BlockHeaderImpl(
      version: Int32,
      previousBlockHash: DoubleSha256Digest,
      merkleRootHash: DoubleSha256Digest,
      time: UInt32,
      nBits: UInt32,
      nonce: UInt32)
      extends BlockHeader

  def apply(
      version: Int32,
      previousBlockHash: DoubleSha256Digest,
      merkleRootHash: DoubleSha256Digest,
      time: UInt32,
      nBits: UInt32,
      nonce: UInt32): BlockHeader = {
    BlockHeaderImpl(version,
                    previousBlockHash,
                    merkleRootHash,
                    time,
                    nBits,
                    nonce)
  }

  def fromBytes(bytes: ByteVector): BlockHeader =
    RawBlockHeaderSerializer.read(bytes)

  /** Return type used to carry around extra information
    * about the difficulty required to mine a block. Unfortunately
    * there is weird corner cases like it being an overflow or negative
    * which is returned by [[https://github.com/bitcoin/bitcoin/blob/2068f089c8b7b90eb4557d3f67ea0f0ed2059a23/src/arith_uint256.cpp#L206 arith_uint256#SetCompact()]] in bitcoin core
    * @param difficulty
    * @param isNegative
    * @param isOverflow
    */
  case class TargetDifficultyHelper(
      difficulty: BigInt,
      isNegative: Boolean,
      isOverflow: Boolean)
}
