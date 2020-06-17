package org.bitcoins.node.networking.peer

import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, CompactSizeUInt}
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node._
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.scalatest.FutureOutcome

import scala.concurrent.{Future, Promise}

class DataMessageHandlerTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeConnectedToBitcoind(test, Some(BitcoindVersion.V19))

  private val junkAddress: BitcoinAddress =
    BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")

  it must "verify OnMerkleBlock callbacks are executed" in {
    param: FixtureParam =>
      val SpvNodeConnectedWithBitcoind(spv, bitcoind) = param

      val resultP: Promise[Boolean] = Promise()

      for {
        // fund bitcoind
        _ <- bitcoind.getNewAddress.flatMap(
          bitcoind.generateToAddress(blocks = 101, _))

        sender <- spv.peerMsgSenderF

        txId <- bitcoind.sendToAddress(junkAddress, 1.bitcoin)
        tx <- bitcoind.getRawTransactionRaw(txId)
        _ <- bitcoind.generateToAddress(blocks = 1, junkAddress)
        merkleBlock <- bitcoind.getTxOutProof(Vector(txId))

        payload1 = MerkleBlockMessage(merkleBlock)
        payload2 = TransactionMessage(tx)

        callback: OnMerkleBlockReceived = (
            _: MerkleBlock,
            _: Vector[Transaction]) => {
          Future {
            resultP.success(true)
            ()
          }
        }

        callbacks = NodeCallbacks.onMerkleBlockReceived(callback)

        dataMessageHandler = DataMessageHandler(dummyChainApi, callbacks)
        _ <- dataMessageHandler.handleDataPayload(payload1, sender)
        _ <- dataMessageHandler.handleDataPayload(payload2, sender)
        result <- resultP.future
      } yield assert(result)
  }

  it must "verify OnBlockReceived callbacks are executed" in {
    param: FixtureParam =>
      val SpvNodeConnectedWithBitcoind(spv, bitcoind) = param

      val resultP: Promise[Boolean] = Promise()

      for {
        // fund bitcoind
        _ <- bitcoind.getNewAddress.flatMap(
          bitcoind.generateToAddress(blocks = 101, _))

        sender <- spv.peerMsgSenderF

        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        block <- bitcoind.getBlockRaw(hash)

        payload = BlockMessage(block)

        callback: OnBlockReceived = (_: Block) => {
          Future {
            resultP.success(true)
            ()
          }
        }

        callbacks = NodeCallbacks.onBlockReceived(callback)

        dataMessageHandler = DataMessageHandler(dummyChainApi, callbacks)
        _ <- dataMessageHandler.handleDataPayload(payload, sender)
        result <- resultP.future
      } yield assert(result)
  }

  it must "verify OnBlockHeadersReceived callbacks are executed" in {
    param: FixtureParam =>
      val SpvNodeConnectedWithBitcoind(spv, bitcoind) = param

      val resultP: Promise[Boolean] = Promise()

      for {
        // fund bitcoind
        _ <- bitcoind.getNewAddress.flatMap(
          bitcoind.generateToAddress(blocks = 101, _))

        sender <- spv.peerMsgSenderF

        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        header <- bitcoind.getBlockHeaderRaw(hash)

        payload = HeadersMessage(CompactSizeUInt.one, Vector(header))

        callback: OnBlockHeadersReceived = (_: Vector[BlockHeader]) => {
          Future {
            resultP.success(true)
            ()
          }
        }

        callbacks = NodeCallbacks.onBlockHeadersReceived(callback)

        dataMessageHandler = DataMessageHandler(dummyChainApi, callbacks)
        _ <- dataMessageHandler.handleDataPayload(payload, sender)
        result <- resultP.future
      } yield assert(result)
  }

  it must "verify OnCompactFilterReceived callbacks are executed" in {
    param: FixtureParam =>
      val SpvNodeConnectedWithBitcoind(spv, rpc) = param
      val bitcoind = rpc.asInstanceOf[BitcoindV19RpcClient]

      val resultP: Promise[Boolean] = Promise()

      for {
        // fund bitcoind
        _ <- bitcoind.getNewAddress.flatMap(
          bitcoind.generateToAddress(blocks = 101, _))

        sender <- spv.peerMsgSenderF

        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)

        payload = CompactFilterMessage(FilterType.Basic,
                                       hash.flip,
                                       filter.filter.bytes)

        callback: OnCompactFiltersReceived = (_: Vector[(
            DoubleSha256Digest,
            GolombFilter)]) => {
          Future {
            resultP.success(true)
            ()
          }
        }

        callbacks = NodeCallbacks.onCompactFilterReceived(callback)

        dataMessageHandler = DataMessageHandler(dummyChainApi, callbacks)
        _ <- dataMessageHandler.handleDataPayload(payload, sender)
        result <- resultP.future
      } yield assert(result)
  }
}
