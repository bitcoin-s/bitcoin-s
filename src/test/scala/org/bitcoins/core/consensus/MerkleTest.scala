package org.bitcoins.core.consensus

import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/24/16.
  */
class MerkleTest extends FlatSpec with MustMatchers {

  "Merkle" must "compute the merkle root for the genesis block" in {
    Merkle.computeBlockMerkleRoot(MainNetChainParams.genesisBlock).hex must be ("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  }
}
