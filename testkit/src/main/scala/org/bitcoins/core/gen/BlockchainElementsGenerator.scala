package org.bitcoins.core.gen

import org.bitcoins.core.consensus.Merkle
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.{EmptyTransaction, Transaction}
import org.scalacheck.Gen

import scala.annotation.tailrec

/**
  * Created by tom on 7/6/16.
  */
sealed abstract class BlockchainElementsGenerator {

  /** Generates a block that contains the given txs, plus some more randomly generated ones */
  def block(txs: Seq[Transaction]): Gen[Block] =
    for {
      randomNum <- Gen.choose(1, 10)
      neededTxs = if ((randomNum - txs.size) >= 0) randomNum else 0
      genTxs <- Gen.listOfN(neededTxs, TransactionGenerators.transaction)
      allTxs = genTxs ++ txs
      header <- blockHeader(allTxs)
    } yield Block(header, allTxs)

  /**
    * Generates a random [[Block]], note that we limit this
    * to 10 transactions currently
    */
  def block: Gen[Block] =
    for {
      header <- blockHeader
      txs <- TransactionGenerators.smallTransactions
    } yield Block(header, txs)

  /** Generates a random [[BlockHeader]] */
  def blockHeader: Gen[BlockHeader] =
    for {
      previousBlockHash <- CryptoGenerators.doubleSha256Digest
      b <- blockHeader(previousBlockHash)
    } yield b

  /** Generates a random [[BlockHeader]] with the specified previousBlockHash */
  def blockHeader(previousBlockHash: DoubleSha256Digest): Gen[BlockHeader] =
    for {
      nBits <- NumberGenerator.uInt32s
      b <- blockHeader(previousBlockHash, nBits)
    } yield b

  /** Generates a random [[BlockHeader]] where you can specify the previousBlockHash and nBits */
  def blockHeader(
      previousBlockHash: DoubleSha256Digest,
      nBits: UInt32): Gen[BlockHeader] =
    for {
      numTxs <- Gen.choose(1, 5)
      txs <- Gen.listOfN(numTxs, TransactionGenerators.transaction)
      header <- blockHeader(previousBlockHash, nBits, txs)
    } yield header

  /** Generates a [[BlockHeader]]] that has the fields set to the given values */
  def blockHeader(
      previousBlockHash: DoubleSha256Digest,
      nBits: UInt32,
      txs: Seq[Transaction]): Gen[BlockHeader] =
    for {
      version <- NumberGenerator.int32s
      merkleRootHash = Merkle.computeMerkleRoot(txs)
      time <- NumberGenerator.uInt32s
      nonce <- NumberGenerator.uInt32s
    } yield
      BlockHeader(version,
                  previousBlockHash,
                  merkleRootHash,
                  time,
                  nBits,
                  nonce)

  /** Generates a [[BlockHeader]] that has a merkle root hash corresponding to the given txs */
  def blockHeader(txs: Seq[Transaction]): Gen[BlockHeader] =
    for {
      previousBlockHash <- CryptoGenerators.doubleSha256Digest
      nBits <- NumberGenerator.uInt32s
      header <- blockHeader(previousBlockHash, nBits, txs)
    } yield header

  /**
    * Generates a chain of valid headers of the size specified by num,
    * 'valid' means their nBits are the same and each header properly
    * references the previous block header's hash
    */
  def validHeaderChain(num: Long): Gen[Seq[BlockHeader]] = {
    blockHeader.flatMap { startHeader =>
      validHeaderChain(num, startHeader)
    }
  }

  def validHeaderChain(
      num: Long,
      startHeader: BlockHeader): Gen[Seq[BlockHeader]] = {
    @tailrec
    def loop(
        remainingHeaders: Long,
        accum: Seq[BlockHeader]): Seq[BlockHeader] = {
      if (remainingHeaders == 0) accum.reverse
      else {
        val nextHeader = buildBlockHeader(accum.head.hash, accum.head.nBits)
        loop(remainingHeaders - 1, nextHeader +: accum)
      }
    }
    loop(num - 1, Seq(startHeader))
  }

  private def buildBlockHeader(
      prevBlockHash: DoubleSha256Digest,
      nBits: UInt32): BlockHeader = {
    //nonce for the unique hash
    val nonce = NumberGenerator.uInt32s.sample.get
    BlockHeader(Int32.one,
                prevBlockHash,
                EmptyTransaction.txId,
                UInt32.one,
                nBits,
                nonce)
  }
}

object BlockchainElementsGenerator extends BlockchainElementsGenerator
