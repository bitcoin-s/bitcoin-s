package org.bitcoins.node.networking.peer

import org.bitcoins.core.config.SigNet
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoindV22
import org.bitcoins.testkit.tor.CachedTor
import org.scalatest.FutureOutcome

import scala.concurrent.{Future, Promise}

class DataMessageHandlerTest extends NodeUnitTest with CachedTor {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoindV22

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNeutrinoNodeConnectedToBitcoindV22(test)(system, getFreshConfig)

  it must "catch errors and not fail when processing an invalid payload" in {
    param: NeutrinoNodeConnectedWithBitcoindV22 =>
      val NeutrinoNodeConnectedWithBitcoindV22(node, _) = param

      val sender = node.peerMsgSenders(0)
      for {
        chainApi <- node.chainApiFromDb()
        dataMessageHandler = DataMessageHandler(chainApi, None)(
          node.executionContext,
          node.nodeAppConfig,
          node.chainConfig)

        // Use signet genesis block header, this should be invalid for regtest
        invalidPayload =
          HeadersMessage(Vector(SigNet.chainParams.genesisBlock.blockHeader))

        // Validate that it causes a failure
        _ <- recoverToSucceededIf[RuntimeException](
          chainApi.processHeaders(invalidPayload.headers))

        // Verify we handle the payload correctly
        _ <- dataMessageHandler.handleDataPayload(invalidPayload, sender)
      } yield succeed
  }

  it must "verify OnBlockReceived callbacks are executed" in {
    param: FixtureParam =>
      val NeutrinoNodeConnectedWithBitcoindV22(node, bitcoind) = param

      val resultP: Promise[Block] = Promise()

      val callback: OnBlockReceived = (block: Block) => {
        Future {
          resultP.success(block)
          ()
        }
      }
      val sender = node.peerMsgSenders(0)

      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        block <- bitcoind.getBlockRaw(hash)

        payload = BlockMessage(block)

        nodeCallbacks = NodeCallbacks.onBlockReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)

        dataMessageHandler =
          DataMessageHandler(genesisChainApi, None)(node.executionContext,
                                                    node.nodeAppConfig,
                                                    node.chainConfig)
        _ <- dataMessageHandler.handleDataPayload(payload, sender, node)
        result <- resultP.future
      } yield assert(result == block)
  }

  it must "verify OnBlockHeadersReceived callbacks are executed" in {
    param: FixtureParam =>
      val NeutrinoNodeConnectedWithBitcoindV22(node, bitcoind) = param

      val resultP: Promise[Vector[BlockHeader]] = Promise()

      val callback: OnBlockHeadersReceived = (headers: Vector[BlockHeader]) => {
        Future {
          if (!resultP.isCompleted) {
            resultP.success(headers)
          }
          ()
        }
      }

      val sender = node.peerMsgSenders(0)
      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        header <- bitcoind.getBlockHeaderRaw(hash)

        payload = HeadersMessage(CompactSizeUInt.one, Vector(header))

        callbacks = NodeCallbacks.onBlockHeadersReceived(callback)

        _ = node.nodeAppConfig.addCallbacks(callbacks)
        dataMessageHandler =
          DataMessageHandler(genesisChainApi, None)(node.executionContext,
                                                    node.nodeAppConfig,
                                                    node.chainConfig)

        _ <- dataMessageHandler.handleDataPayload(payload, sender)
        result <- resultP.future
      } yield assert(result == Vector(header))
  }

  it must "verify OnCompactFilterReceived callbacks are executed" in {
    param: FixtureParam =>
      val NeutrinoNodeConnectedWithBitcoindV22(node, bitcoind) = param

      val resultP: Promise[Vector[(DoubleSha256Digest, GolombFilter)]] =
        Promise()
      val callback: OnCompactFiltersReceived = {
        (filters: Vector[(DoubleSha256Digest, GolombFilter)]) =>
          Future {
            resultP.success(filters)
            ()
          }
      }
      val sender = node.peerMsgSenders(0)
      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)

        payload =
          CompactFilterMessage(FilterType.Basic, hash.flip, filter.filter.bytes)

        nodeCallbacks = NodeCallbacks.onCompactFilterReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        dataMessageHandler =
          DataMessageHandler(genesisChainApi, None)(node.executionContext,
                                                    node.nodeAppConfig,
                                                    node.chainConfig)

        _ <- dataMessageHandler.handleDataPayload(payload, sender)
        result <- resultP.future
      } yield assert(result == Vector((hash.flip, filter.filter)))
  }

  it must "verify OnTxReceived callbacks are executed" in {
    param: FixtureParam =>
      val NeutrinoNodeConnectedWithBitcoindV22(node, bitcoind) = param

      val resultP: Promise[Transaction] = Promise()

      val callback: OnTxReceived = (tx: Transaction) => {
        Future {
          resultP.success(tx)
          ()
        }
      }
      val sender = node.peerMsgSenders(0)

      for {

        txId <- bitcoind.sendToAddress(junkAddress, 1.bitcoin)
        tx <- bitcoind.getRawTransactionRaw(txId)

        payload = TransactionMessage(tx)

        nodeCallbacks = NodeCallbacks.onTxReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)

        dataMessageHandler =
          DataMessageHandler(genesisChainApi, None)(node.executionContext,
                                                    node.nodeAppConfig,
                                                    node.chainConfig)
        _ <- dataMessageHandler.handleDataPayload(payload, sender, node)
        result <- resultP.future
      } yield assert(result == tx)
  }
}
