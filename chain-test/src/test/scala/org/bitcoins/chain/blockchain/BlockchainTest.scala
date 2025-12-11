package org.bitcoins.chain.blockchain

import org.bitcoins.chain.validation.TipUpdateResult
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  BlockHeader,
  RegTestNetChainParams
}
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkitcore.chain.ChainTestUtil

import scala.collection.mutable

class BlockchainTest extends BitcoinSAsyncTest {
  behavior of "Blockchain"

  it must "have the correct toString" in {
    val genesis = ChainTestUtil.regTestGenesisHeaderDb
    val headerDb =
      BlockHeaderHelper.buildNextHeader(genesis)
    val chain = Blockchain(Vector(headerDb, genesis))

    assert(chain.toString == s"BaseBlockchain(tip=$headerDb,length=2)")
  }

  it must "connect a new header to the current tip of a blockchain" in {
    val blockchain = Blockchain.fromHeaders(
      headers = Vector(ChainTestUtil.regTestGenesisHeaderDb)
    )

    val newHeader =
      BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)

    val connectTip =
      Blockchain.connectTip(header = newHeader.blockHeader,
                            blockchain = blockchain,
                            chainParams = RegTestNetChainParams)

    connectTip match {
      case ConnectTipResult.ExtendChain(_, newChain) =>
        assert(newHeader == newChain.tip)

      case _ @(_: ConnectTipResult.Reorg | _: ConnectTipResult.BadTip) =>
        fail()
    }
  }

  it must "reconstruct a blockchain given a child header correctly" in {
    val accum = new mutable.ArrayBuffer[BlockHeaderDb](5)
    accum.+=(ChainTestUtil.regTestGenesisHeaderDb)
    // generate 4 headers
    0.until(4).foreach { _ =>
      val newHeader = BlockHeaderHelper.buildNextHeader(accum.last)
      accum.+=(newHeader)
    }

    // now given the last header, and the other headers we should reconstruct the blockchain
    val headers = accum.dropRight(1).toVector
    val tip = accum.last

    val reconstructed = Blockchain.reconstructFromHeaders(
      childHeader = tip,
      ancestors = headers,
      chainParams = RegTestNetChainParams
    )

    assert(reconstructed.length == 1)
    val chain = reconstructed.head
    assert(chain.toVector.length == 5)
    assert(chain.tip == accum.last)
    assert(chain.last == ChainTestUtil.regTestGenesisHeaderDb)
    assert(chain.toVector == accum.reverse.toVector)
  }

  it must "fail to reconstruct a blockchain if we do not have validly connected headers" in {
    val missingHeader =
      BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)

    val thirdHeader = BlockHeaderHelper.buildNextHeader(missingHeader)

    val reconstructed =
      Blockchain.reconstructFromHeaders(
        thirdHeader,
        Vector(ChainTestUtil.regTestGenesisHeaderDb),
        chainParams = RegTestNetChainParams
      )

    assert(reconstructed.isEmpty)
  }

  it must "fail to create a BlockchainUpdate.Failed with incompatible successful headers" in {
    val genesis = ChainTestUtil.regTestGenesisHeaderDb
    val second = BlockHeaderHelper.buildNextHeader(genesis)
    val chain = Blockchain(Vector(second, genesis))

    assertThrows[IllegalArgumentException] {
      BlockchainUpdate.Failed(
        chain,
        Vector(genesis),
        second.blockHeader,
        TipUpdateResult.BadNonce(second.blockHeader)
      )
    }
  }

  it must "correctly calculate a BlockchainUpdate.Success's height" in {
    val genesis = ChainTestUtil.regTestGenesisHeaderDb
    val second = BlockHeaderHelper.buildNextHeader(genesis)
    val chain = Blockchain(Vector(second, genesis))

    val updated = BlockchainUpdate.Successful(chain, chain.toVector)

    assert(updated.height == chain.height)
  }

  it must "correctly identify a bad tip" in {
    val genesis = ChainTestUtil.regTestGenesisHeaderDb
    val chain = Blockchain(Vector(genesis))

    val goodHeader = BlockHeaderHelper.buildNextHeader(genesis).blockHeader
    val badHeader = BlockHeader(
      version = goodHeader.version,
      previousBlockHash = goodHeader.previousBlockHash,
      merkleRootHash = goodHeader.merkleRootHash,
      time = goodHeader.time,
      nBits = UInt32.zero,
      nonce = goodHeader.nonce
    )

    val result = Blockchain.connectTip(header = badHeader,
                                       blockchain = chain,
                                       chainParams = RegTestNetChainParams)

    assert(result.isInstanceOf[ConnectTipResult.BadTip])
  }
}
