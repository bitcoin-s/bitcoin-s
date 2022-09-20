package org.bitcoins.chain

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256DigestBE}
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainDbUnitTest,
  ChainUnitTest
}
import org.scalatest.FutureOutcome
import scodec.bits.ByteVector

import scala.concurrent.{Future, Promise}

class ChainCallbacksTest extends ChainDbUnitTest {
  override type FixtureParam = ChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandlerGenesisFilter(test)

  it must "process a new valid block header with a callback" in {
    chainHandler: ChainHandler =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnBlockHeaderConnected = {
        case _: Vector[(Int, BlockHeader)] => {
          Future {
            resultP.success(true)
            ()
          }
        }
      }

      val callbacks = ChainCallbacks.onBlockHeaderConnected(callback)
      chainHandler.chainConfig.addCallbacks(callbacks)

      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      for {
        _ <- chainHandler.processHeader(newValidHeader.blockHeader)
        result <- resultP.future
      } yield assert(result)
  }

  it must "process a new valid compact filter header with a callback" in {
    chainHandler: ChainHandler =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnCompactFilterHeaderConnected = {
        case _: Vector[CompactFilterHeaderDb] => {
          Future {
            resultP.success(true)
            ()
          }
        }
      }

      val callbacks = ChainCallbacks.onCompactFilterHeaderConnected(callback)
      chainHandler.chainConfig.addCallbacks(callbacks)

      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)
      val nextCompactFilterHeaderDb =
        CompactFilterHeaderDb(
          hashBE = DoubleSha256DigestBE.fromHex(
            "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"),
          previousFilterHeaderBE = ChainUnitTest.genesisFilterHeaderDb.hashBE,
          height = 1,
          filterHashBE = DoubleSha256DigestBE.fromHex(
            "555152535455565758595a5b5c5d5e5f555152535455565758595a5b5c5d5e5f"),
          blockHashBE = newValidHeader.hashBE
        )

      for {
        _ <- chainHandler.processHeader(newValidHeader.blockHeader)
        _ <- chainHandler.processFilterHeader(
          nextCompactFilterHeaderDb.filterHeader,
          nextCompactFilterHeaderDb.blockHashBE)
        result <- resultP.future
      } yield assert(result)
  }

  it must "process a new valid compact filter with a callback" in {
    chainHandler: ChainHandler =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnCompactFilterConnected = {
        case _: Vector[CompactFilterDb] => {
          Future {
            resultP.success(true)
            ()
          }
        }
      }

      val callbacks = ChainCallbacks.onCompactFilterConnected(callback)
      chainHandler.chainConfig.addCallbacks(callbacks)

      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      val bytes = ByteVector(scala.util.Random.nextBytes(32))
      val hashBE = CryptoUtil.doubleSHA256(bytes)
      val nextCompactFilterHeaderDb =
        CompactFilterHeaderDb(
          hashBE = DoubleSha256DigestBE.fromHex(
            "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"),
          previousFilterHeaderBE = ChainUnitTest.genesisFilterHeaderDb.hashBE,
          height = 1,
          filterHashBE = hashBE.flip,
          blockHashBE = newValidHeader.hashBE
        )

      val nextCompactFilter = CompactFilterDb(
        hashBE = hashBE.flip,
        filterType = FilterType.Basic,
        bytes = bytes,
        height = 1,
        blockHashBE = nextCompactFilterHeaderDb.blockHashBE
      )

      val filterMessage =
        CompactFilterMessage(nextCompactFilter.blockHashBE.flip,
                             nextCompactFilter.golombFilter)
      for {
        _ <- chainHandler.processHeader(newValidHeader.blockHeader)
        _ <- chainHandler.processFilterHeader(
          nextCompactFilterHeaderDb.filterHeader,
          nextCompactFilterHeaderDb.blockHashBE)
        _ <- chainHandler.processFilter(filterMessage)
        result <- resultP.future
      } yield assert(result)
  }
}
