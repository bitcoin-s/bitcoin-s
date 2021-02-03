package org.bitcoins.core.protocol.blockchain

import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkit.core.gen.MerkleGenerator
import org.scalacheck.{Prop, Properties}

/** Created by chris on 2/13/17.
  */
class PartialMerkleTreeSpec extends Properties("PartialMerkleTreeSpec") {

  property(
    "must be able to extract all of the txids we indicated to be matches") =
    Prop.forAll(MerkleGenerator.partialMerkleTree) {
      case (partialMerkleTree: PartialMerkleTree,
            txMatches: Seq[(Boolean, DoubleSha256Digest)]) =>
        val matchedTxs = txMatches.filter(_._1).map(_._2)
        partialMerkleTree.extractMatches == matchedTxs
    }

  property(
    "must generate the same partial merkle tree from the same parameters") =
    Prop.forAll(MerkleGenerator.partialMerkleTree) {
      case (partialMerkleTree: PartialMerkleTree, _) =>
        val partialMerkleTree2 =
          PartialMerkleTree(partialMerkleTree.transactionCount,
                            partialMerkleTree.hashes,
                            partialMerkleTree.bits)
        partialMerkleTree2 == partialMerkleTree
    }

}
