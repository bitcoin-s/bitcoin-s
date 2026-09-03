package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.number.UInt32
import org.bitcoins.testkitcore.gen.BlockchainElementsGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by tom on 7/6/16.
  */
class BlockHeaderSpec extends BitcoinSUnitTest {

  it must "serialization symmetry" in {
    forAll(BlockchainElementsGenerator.blockHeader) { header =>
      assert(BlockHeader(header.hex) == header)
    }
  }

  it must "return zero block proof for a zero target" in {
    // Bitcoin Core's GetBlockProof explicitly checks
    // `fNegative || fOverflow || bnTarget == 0` before dividing
    // (pow.cpp) -- with nBits encoding a target of exactly zero, dividing
    // by (target + 1) = 1 would otherwise wrongly produce the enormous
    // value 2^256 instead of the correct proof-of-work contribution of zero.
    val genesisHeader = MainNetChainParams.genesisBlock.blockHeader
    val zeroTargetHeader = BlockHeader(
      genesisHeader.version,
      genesisHeader.previousBlockHash,
      genesisHeader.merkleRootHash,
      genesisHeader.time,
      UInt32.zero,
      genesisHeader.nonce
    )

    BlockHeader.getBlockProof(zeroTargetHeader) must be(BigInt(0))
  }
}
