package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gen.MerkleGenerator
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 8/12/16.
  */
class MerkleBlockSpec extends Properties("MerkleBlockSpec") with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(MerkleGenerator.merkleBlockWithInsertedTxIds) {
      case (merkleBlock: MerkleBlock, block: Block, txIds : Seq[DoubleSha256Digest]) =>
        MerkleBlock(merkleBlock.hex) == merkleBlock
    }

  property("contains all inserted txids when we directly create a merkle block from the txids") =
    Prop.forAllNoShrink(MerkleGenerator.merkleBlockWithInsertedTxIds) {
      case (merkleBlock: MerkleBlock, block: Block, txIds: Seq[DoubleSha256Digest]) =>
        val extractedMatches = merkleBlock.partialMerkleTree.extractMatches
        extractedMatches == txIds
    }

  property("contains all txids matched by a bloom filter") = {
    Prop.forAllNoShrink(MerkleGenerator.merkleBlockCreatedWithBloomFilter) {
      case (merkleBlock: MerkleBlock, block: Block, txIds: Seq[DoubleSha256Digest], loadedFilter: BloomFilter) =>
        val extractedMatches = merkleBlock.partialMerkleTree.extractMatches
        //note that intersection is paramount here, our bloom filter can have false positives
        //so we cannot do a straight up equality comparison
        //bloom filters cannot have false negatives though, so we should have ATLEAST
        //the txids specified by our generator in this set
        extractedMatches.intersect(txIds) == txIds
    }
  }
}