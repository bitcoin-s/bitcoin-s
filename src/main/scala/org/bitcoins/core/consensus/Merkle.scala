package org.bitcoins.core.consensus

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil}

import scala.annotation.tailrec

/**
  * Created by chris on 5/24/16.
  * This trait contains all functionality related to computing merkle trees
  * Mimics this functionality inside of bitcoin core
  * https://github.com/bitcoin/bitcoin/blob/master/src/consensus/merkle.cpp
  */
trait Merkle extends BitcoinSLogger {
  /**
    * Computes the merkle root for the given block
    * @param block the given block that needs the merkle root computed
    * @return the hash representing the merkle root for this block
    */
  def computeBlockMerkleRoot(block : Block) : DoubleSha256Digest = computeMerkleRoot(block.transactions)

  /**
    * Computes the merkle root of a given sequence of hashes
    * This hash function assumes that all of the hashes are big endian encoded
    * @param hashes the big endian encoded hashes from which the merkle root is derived from
    * @param accum the accumulator for the recursive call on this function - more than likely you do not need to worry about this
    * @return the merkle root
    */
  @tailrec
  final def computeMerkleRoot(hashes : Seq[DoubleSha256Digest], accum : List[DoubleSha256Digest] = List()) : DoubleSha256Digest = hashes match {
    case Nil =>
      logger.debug("No more hashes to check")
      if (accum.size == 1) DoubleSha256Digest(accum.head.bytes.reverse)
      else if (accum.size == 0) throw new RuntimeException("We cannot have zero hashes and nothing in the accumulator, this means there is NO transaction in the block. " +
        "There always should be ATLEAST one - the coinbase tx")
      else computeMerkleRoot(accum.reverse, List())
    case h :: h1 :: t =>
      logger.debug("We have an even amount of txids")
      logger.debug("Hashes: " + hashes.map(_.hex))
      val hash = CryptoUtil.doubleSHA256(h.bytes ++ h1.bytes)
      computeMerkleRoot(t, hash :: accum)
    case h :: t =>
      logger.debug("We have an odd amount of txids")
      logger.debug("Hashes: " + hashes.map(_.hex))
      //means that we have an odd amount of txids, this means we duplicate the last hash in the tree
      val hash = CryptoUtil.doubleSHA256(h.bytes ++ h.bytes)
      computeMerkleRoot(t,hash :: accum)
  }

  /**
    * Computes the merkle root for the given sequence of transactions
    * @param transactions the list of transactions whose merkle root needs to be computed
    * @return the merkle root for the sequence of transactions
    */
  def computeMerkleRoot(transactions : Seq[Transaction]) : DoubleSha256Digest = transactions match {
    case Nil => throw new RuntimeException("We cannot have zero transactions in the block. There always should be ATLEAST one - the coinbase tx")
    case h :: Nil => h.txId
    case h :: t =>
      //we need to switch our txIds from little endian encoding to big endian encoding for computing merkle trees
      val txHashes = transactions.map(tx => DoubleSha256Digest(tx.txId.bytes.reverse))
      computeMerkleRoot(txHashes)
  }
}

object Merkle extends Merkle

