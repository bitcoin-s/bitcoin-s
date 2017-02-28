package org.bitcoins.core.gen

import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.{Block, MerkleBlock, PartialMerkleTree}
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.Gen

/**
  * Created by chris on 8/12/16.
  */
trait MerkleGenerator extends BitcoinSLogger {

  /** Returns a [[MerkleBlock]] including the sequence of hashes inserted in to the bloom filter */
  def merkleBlockWithInsertedTxIds: Gen[(MerkleBlock,Block,Seq[DoubleSha256Digest])] = for {
    block <- BlockchainElementsGenerator.block
    txIds <- Gen.someOf(block.transactions.map(_.txId))
    merkleBlock = MerkleBlock(block,txIds)
  } yield (merkleBlock, block, txIds)


  /** Returns a [[MerkleBlock]] created with a [[org.bitcoins.core.bloom.BloomFilter]], with the block it was created from
    * and the transactions that were matched inside of that block
    * NOTE: Since bloom filters can produce false positives, it is possible that there will be
    * matches in the parital merkle tree that SHOULD NOT be matched. Bloom filters do not guaratnee no
    * false negatives.
    * @return
    */
  def merkleBlockCreatedWithBloomFilter: Gen[(MerkleBlock, Block,Seq[DoubleSha256Digest], BloomFilter)] = for {
    block <- BlockchainElementsGenerator.block
    //choose some random txs in the block to put in the bloom filter
    txIds <- Gen.someOf(block.transactions.map(_.txId))
    initialFilter <- BloomFilterGenerator.bloomFilter(txIds.map(_.bytes))
    (merkleBlock,loadedFilter) = MerkleBlock(block,initialFilter)
  } yield (merkleBlock,block,txIds,loadedFilter)

  /** Generates a partial merkle tree with a sequence of txids and a flag indicating if the txid was matched */
  def partialMerkleTree: Gen[(PartialMerkleTree, Seq[(Boolean,DoubleSha256Digest)])] = for {
    randomNum <- Gen.choose(1,25)
    txMatches <- txIdsWithMatchIndication(randomNum)
  } yield (PartialMerkleTree(txMatches),txMatches)

  /** Generates a transaction ids with a boolean indicator if they match the bloom filter or not
    * this is useful for testing partial merkle trees as this is how they are built.
    * @return
    */
  private def txIdWithMatchIndication: Gen[(Boolean,DoubleSha256Digest)] = for {
    hash <- CryptoGenerators.doubleSha256Digest
    bool <- Gen.choose(0,1)
  } yield (bool == 1, hash)

  /** Generates a list of txids with a boolean indicator signifying if it matched the bloom filter or not */
  def txIdsWithMatchIndication(num: Int): Gen[Seq[(Boolean,DoubleSha256Digest)]] = Gen.listOfN(num,txIdWithMatchIndication)
}

object MerkleGenerator extends MerkleGenerator
