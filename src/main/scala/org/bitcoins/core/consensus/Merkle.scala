package org.bitcoins.core.consensus

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.CryptoUtil

import scala.annotation.tailrec

/**
  * Created by chris on 5/24/16.
  * This trait contains all functionality related to computing merkle trees
  * Mimics this functionality inside of bitcoin core
  * https://github.com/bitcoin/bitcoin/blob/master/src/consensus/merkle.cpp
  */
trait Merkle {
  /**
    * Computes the merkle root for the given block
    * @param block the given block that needs the merkle root computed
    * @return the hash representing the merkle root for this block
    */
  def computeBlockMerkleRoot(block : Block) : DoubleSha256Digest = {
    val txHashes = block.transactions.map(_.txId)
    @tailrec
    def loop(hashes : Seq[DoubleSha256Digest], accum : List[DoubleSha256Digest]) : DoubleSha256Digest = hashes match {
      case Nil =>
        if (accum.size == 1) accum.head
        else if (accum.size == 0) ???
        else loop(accum, List())
      case h :: h1 :: t =>
        val hash = CryptoUtil.doubleSHA256(h.bytes ++ h1.bytes)
        loop(t, hash :: accum)
      case h :: t =>
        //means that we have an odd amount of txids, this means we duplicate the last hash in the tree
        val hash = CryptoUtil.doubleSHA256(h.bytes ++ h.bytes)
        loop(t,hash :: accum)
    }
    loop(txHashes,List())
  }

}

object Merkle extends Merkle

