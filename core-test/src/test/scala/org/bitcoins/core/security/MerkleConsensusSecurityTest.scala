package org.bitcoins.core.security

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  MainNetChainParams,
  PartialMerkleTree
}
import org.bitcoins.core.serializers.blockchain.RawMerkleBlockSerializer
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.{BitVector, ByteVector}

/** Security reproduction tests for merkle/block/consensus helper findings.
  * Every test asserts the CORRECT/SAFE behavior, so each one FAILS until the
  * corresponding bug is fixed. Production code is intentionally untouched.
  */
class MerkleConsensusSecurityTest extends BitcoinSUnitTest {

  "MerkleConsensusSecurity" must "reject crafted merkleblock inputs with a clean error instead of crashing or accepting them" in {
    // Finding 1 (Medium): PartialMerkleTree reconstruct crashes with raw
    // `.head` exceptions and lacks Bitcoin Core's sanity caps
    // (core/src/main/scala/org/bitcoins/core/protocol/blockchain/PartialMerkleTree.scala:72,124,325,330,364;
    // core/src/main/scala/org/bitcoins/core/serializers/blockchain/RawMerkleBlockSerializer.scala:19-46).
    // Correct behavior: zero/absurd transaction counts and truncated
    // bits/hashes are rejected cleanly (IllegalArgumentException), like
    // Bitcoin Core's merkleblock deserialization/ExtractMatches checks.
    val h = DoubleSha256Digest(
      "01272b2b1c8c33a1b4e9ab111db41c9ac275e686fbd9c5d482e586d03e9e0552"
    )

    // zero transactions: calcMaxHeight(0) degenerates and the traversal
    // crashes on `.head` of the empty bits/hashes instead of rejecting
    intercept[IllegalArgumentException] {
      PartialMerkleTree(UInt32.zero, Vector.empty, BitVector.empty)
    }

    // absurd transaction count: Bitcoin Core rejects
    // nTransactions > MAX_BLOCK_WEIGHT / MIN_TRANSACTION_WEIGHT (= 66,666)
    intercept[IllegalArgumentException] {
      PartialMerkleTree(UInt32(100000L), Vector.empty, BitVector.low(8))
    }

    // fewer hashes than the traversal consumes: crashes with `.head` on the
    // empty hash vector (root bit set forces traversal into both children)
    val rootSetBits =
      BitVector.bits(Seq(true, false, false, false, false, false, false, false))
    intercept[IllegalArgumentException] {
      PartialMerkleTree(UInt32.two, Vector(h), rootSetBits)
    }

    // fewer bits than the traversal consumes: all-true flags force recursion
    // past the end of the 8 provided bits, crashing on `.head`
    intercept[IllegalArgumentException] {
      PartialMerkleTree(UInt32(5), Vector.fill(8)(h), BitVector.high(8))
    }

    // the same zero-transaction case parsed off the wire as a merkleblock
    // message must be rejected cleanly as well
    val merkleBlockBytes = {
      val headerBytes = MainNetChainParams.genesisBlock.blockHeader.bytes
      val txCountLE = ByteVector.low(4) // nTransactions = 0
      val hashCount = ByteVector(0.toByte)
      val flagCount = ByteVector(1.toByte)
      val flags = ByteVector(0.toByte)
      headerBytes ++ txCountLE ++ hashCount ++ flagCount ++ flags
    }
    intercept[IllegalArgumentException] {
      RawMerkleBlockSerializer.read(merkleBlockBytes)
    }
  }

  it must "compute calcMaxHeight with integer-exact results for large transaction counts" in {
    // Finding 3 (Low): float-based calcMaxHeight disagrees with Bitcoin
    // Core's integer loop at n = 2^29 and n = 2^31
    // (core/src/main/scala/org/bitcoins/core/protocol/blockchain/PartialMerkleTree.scala:391-392).
    // Correct behavior: exact powers of two return their exponent (Core's
    // CalcTreeWidth loop gives 29 and 31); the log2 double computation
    // rounds to 30 and 32.
    //
    // Note: 1 << 31 overflows as a 32-bit Int (wraps to Int.MinValue), so
    // calcMaxHeight must accept a Long to even be able to represent 2^31 as
    // an argument; 1L << 31 avoids that overflow by doing the shift in
    // 64-bit Long arithmetic from the start.
    PartialMerkleTree.calcMaxHeight(1 << 29) must be(29)
    PartialMerkleTree.calcMaxHeight(1L << 31) must be(31)
  }
}
