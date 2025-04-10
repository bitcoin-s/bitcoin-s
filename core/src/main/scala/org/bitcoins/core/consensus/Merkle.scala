package org.bitcoins.core.consensus

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{
  NonWitnessTransaction,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.util._
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256Digest}

import scala.annotation.tailrec

/** Created by chris on 5/24/16. This trait contains all functionality related
  * to computing merkle trees Mimics this functionality inside of bitcoin core
  * [[https://github.com/bitcoin/bitcoin/blob/master/src/consensus/merkle.cpp]]
  */
trait Merkle {

  type MerkleTree = BinaryTreeDoubleSha256Digest

  /** Computes the merkle root for the given block
    * @param block
    *   the given block that needs the merkle root computed
    * @return
    *   the hash representing the merkle root for this block
    */
  def computeBlockMerkleRoot(block: Block): DoubleSha256Digest =
    computeMerkleRoot(block.transactions)

  def computeMerkleRoot(
      transactions: Vector[Transaction]): DoubleSha256Digest = {
    val result = if (transactions.isEmpty) {
      throw new IllegalArgumentException(
        "We cannot have zero transactions in the block. There always should be ATLEAST one - the coinbase tx")
    } else if (transactions.length == 1) {
      transactions.head.txId
    } else {
      val leafs = transactions.map(tx => LeafDoubleSha256Digest(tx.txId))
      val merkleTree = build(leafs, Vector.empty)
      merkleTree.value.get
    }
    result
  }

  /** Builds a [[MerkleTree]] from sequence of sub merkle trees. This subTrees
    * can be individual txids (leafs) or full blown subtrees
    * @param subTrees
    *   the trees that need to be hashed
    * @param accum
    *   the accumulated merkle trees, waiting to be hashed next round
    * @return
    *   the entire Merkle tree computed from the given merkle trees
    */
  @tailrec
  final def build(
      subTrees: Vector[MerkleTree],
      accum: Vector[MerkleTree]): MerkleTree = {
    if (subTrees.isEmpty) {
      if (accum.length == 1) accum.head
      else if (accum.isEmpty) {
        throw new IllegalArgumentException(
          "Should never have sub tree size of zero, this implies there was zero hashes given")
      } else {
        build(accum.reverse, Vector.empty)
      }
    } else if (subTrees.length >= 2) {
      val newTree = computeTree(subTrees.head, subTrees(1))
      build(subTrees.drop(2), newTree +: accum)
    } else {
      // means that we have an odd amount of txids, this means we duplicate the last hash in the tree
      val newTree = computeTree(subTrees.head, subTrees.head)
      build(subTrees.drop(1), newTree +: accum)
    }
  }

  /** Builds a merkle tree from a sequence of hashes */
  def build(hashes: Vector[DoubleSha256Digest]): MerkleTree = {
    val leafs = hashes.map(LeafDoubleSha256Digest.apply)
    build(leafs, Vector.empty)
  }

  /** Computes the merkle tree of two sub merkle trees */
  def computeTree(tree1: MerkleTree, tree2: MerkleTree): MerkleTree = {
    val bytes = tree1.value.get.bytes ++ tree2.value.get.bytes
    val hash = CryptoUtil.doubleSHA256(bytes)
    NodeDoubleSha256Digest(hash, tree1, tree2)
  }

  /** Computes the commitment of the block to the wtxids See the definition of a
    * block commitment in BIP141
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#commitment-structure]]
    * [[https://github.com/bitcoin/bitcoin/blob/7490ae8b699d2955b665cf849d86ff5bb5245c28/src/consensus/merkle.cpp#L168]]
    */
  def computeBlockWitnessMerkleTree(block: Block): MerkleTree = {
    val coinbaseWTxId = DoubleSha256Digest.empty
    val hashes = block.transactions.tail.map {
      case wtx: WitnessTransaction    => wtx.wTxId
      case btx: NonWitnessTransaction => btx.txId
    }
    build(coinbaseWTxId +: hashes)
  }

  /** Computes the merkle root for the committment inside of a coinbase txs
    * scriptPubKey See BIP141
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#commitment-structure]]
    */
  def computeBlockWitnessMerkleRoot(block: Block): DoubleSha256Digest = {
    computeBlockWitnessMerkleTree(block).value.get
  }
}

object Merkle extends Merkle
