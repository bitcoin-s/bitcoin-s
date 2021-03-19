package org.bitcoins.rpc

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.rpc.util.{NodePair, RpcUtil}
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.{FutureOutcome, Outcome}

import java.io.File
import scala.concurrent.Future

class TestRpcUtilTest extends BitcoindFixturesCachedPairNewest {

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      clients <- clientsF
      outcome = with2BitcoindsCached(test, clients)
      fut <- outcome.toFuture
    } yield fut

    new FutureOutcome(outcomeF)
  }

  behavior of "BitcoindRpcUtil"

  it should "create a temp bitcoin directory when creating a DaemonInstance, and then delete it" in {
    _: NodePair[BitcoindV21RpcClient] =>
      val instance =
        BitcoindRpcTestUtil.instance(RpcUtil.randomPort, RpcUtil.randomPort)
      val dir = instance.datadir
      assert(dir.isDirectory)
      assert(dir.getPath().startsWith(scala.util.Properties.tmpDir))
      assert(
        dir.listFiles.contains(new File(dir.getAbsolutePath + "/bitcoin.conf")))
      FileUtil.deleteTmpDir(dir)
      assert(!dir.exists)
  }

  it should "be able to generate and sync blocks" in {
    nodes: NodePair[BitcoindV21RpcClient] =>
      val NodePair(first, second) = nodes
      for {
        address <- second.getNewAddress
        txid <- first.sendToAddress(address, Bitcoins.one)
        _ <- BitcoindRpcTestUtil.generateAndSync(Vector(first, second))
        tx <- first.getTransaction(txid)
        _ = assert(tx.confirmations > 0)
        rawTx <- second.getRawTransaction(txid)
        _ = assert(rawTx.confirmations.exists(_ > 0))
        firstBlock <- first.getBestBlockHash
        secondBlock <- second.getBestBlockHash
      } yield assert(firstBlock == secondBlock)
  }

  it should "ble able to generate blocks with multiple clients and sync inbetween" in {
    nodes: NodePair[BitcoindV21RpcClient] =>
      val blocksToGenerate = 10
      val NodePair(first, second) = nodes
      val allClients = nodes.toVector
      for {
        heightPreGeneration <- first.getBlockCount
        _ <- BitcoindRpcTestUtil.generateAllAndSync(allClients,
                                                    blocks = blocksToGenerate)
        firstHash <- first.getBestBlockHash
        secondHash <- second.getBestBlockHash
        heightPostGeneration <- first.getBlockCount
      } yield {
        assert(firstHash == secondHash)
        assert(
          heightPostGeneration - heightPreGeneration == blocksToGenerate * allClients.length)
      }
  }

  it should "be able to find outputs of previous transactions" in {
    nodes: NodePair[BitcoindV21RpcClient] =>
      val NodePair(first, second) = nodes
      for {
        address <- second.getNewAddress
        txid <- first.sendToAddress(address, Bitcoins.one)
        hashes <- BitcoindRpcTestUtil.generateAndSync(Vector(first, second))
        vout <- BitcoindRpcTestUtil.findOutput(first,
                                               txid,
                                               Bitcoins.one,
                                               Some(hashes.head))
        tx <- first.getRawTransaction(txid, Some(hashes.head))
      } yield {
        assert(tx.vout(vout.toInt).value == Bitcoins.one)
      }
  }
}
