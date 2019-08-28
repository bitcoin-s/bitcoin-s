package org.bitcoins.rpc

import java.io.File

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.util.AsyncUtil.RpcRetryException
import org.bitcoins.rpc.util.{AsyncUtil, RpcUtil}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{BitcoindRpcTest, FileUtil}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Success

class TestRpcUtilTest extends BitcoindRpcTest {

  private lazy val clientsF =
    BitcoindRpcTestUtil.createNodeTriple(clientAccum = clientAccum)

  private def trueLater(delay: Int = 1000): Future[Boolean] = Future {
    Thread.sleep(delay)
    true
  }

  private def boolLaterDoneAnd(
      bool: Boolean,
      boolFuture: Future[Boolean]): Future[Boolean] = {
    Future.successful(boolFuture.value.contains(Success(bool)))
  }

  private def boolLaterDoneAndTrue(
      trueLater: Future[Boolean]): () => Future[Boolean] = { () =>
    boolLaterDoneAnd(bool = true, trueLater)
  }

  behavior of "TestRpcUtil"

  it should "complete immediately if condition is true" in {
    AsyncUtil
      .retryUntilSatisfiedF(conditionF = () => Future.successful(true),
                            duration = 0.millis)
      .map { _ =>
        succeed
      }
  }

  it should "fail if condition is false" in {
    recoverToSucceededIf[RpcRetryException] {
      AsyncUtil.retryUntilSatisfiedF(
        conditionF = () => Future.successful(false),
        duration = 0.millis)
    }
  }

  it should "succeed after a delay" in {
    val boolLater = trueLater(delay = 250)
    AsyncUtil.retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater)).map { _ =>
      succeed
    }
  }

  it should "fail if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    recoverToSucceededIf[RpcRetryException] {
      AsyncUtil
        .retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater),
                              duration = 0.millis)
        .map(_ => succeed)
    }
  }

  it should "succeed immediately if condition is true" in {
    AsyncUtil
      .awaitCondition(condition = () => true, 0.millis)
      .map(_ => succeed)

  }

  it should "timeout if condition is false" in {
    recoverToSucceededIf[RpcRetryException] {
      AsyncUtil
        .awaitCondition(condition = () => false, duration = 0.millis)
        .map(_ => succeed)
    }
  }

  it should "wait for a delay and then succeed" in {
    val boolLater = trueLater(delay = 250)
    val before: Long = System.currentTimeMillis
    AsyncUtil.awaitConditionF(boolLaterDoneAndTrue(boolLater)).flatMap { _ =>
      val after: Long = System.currentTimeMillis
      assert(after - before >= 250)
    }
  }

  it should "timeout if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    recoverToSucceededIf[RpcRetryException] {
      AsyncUtil
        .awaitConditionF(boolLaterDoneAndTrue(boolLater), duration = 0.millis)
        .map(_ => succeed)
    }
  }

  behavior of "BitcoindRpcUtil"

  it should "create a temp bitcoin directory when creating a DaemonInstance, and then delete it" in {
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

  it should "be able to create a single node, wait for it to start and then delete it" in {
    val instance = BitcoindRpcTestUtil.instance()
    val client = BitcoindRpcClient.withActorSystem(instance)
    val startedF = client.start()

    startedF.map { _ =>
      client.stop()
      succeed
    }
  }

  it should "be able to create a connected node pair with more than 100 blocks and then delete them" in {
    for {
      (client1, client2) <- BitcoindRpcTestUtil.createNodePair()
      _ = assert(client1.getDaemon.datadir.isDirectory)
      _ = assert(client2.getDaemon.datadir.isDirectory)

      nodes <- client1.getAddedNodeInfo(client2.getDaemon.uri)
      _ = assert(nodes.nonEmpty)

      count1 <- client1.getBlockCount
      count2 <- client2.getBlockCount
      _ = assert(count1 > 100)
      _ = assert(count2 > 100)
      _ <- BitcoindRpcTestUtil.deleteNodePair(client1, client2)
    } yield {
      assert(!client1.getDaemon.datadir.exists)
      assert(!client2.getDaemon.datadir.exists)
    }
  }

  it should "be able to generate and sync blocks" in {
    for {
      (first, second, third) <- clientsF
      address <- second.getNewAddress
      txid <- first.sendToAddress(address, Bitcoins.one)
      _ <- BitcoindRpcTestUtil.generateAndSync(Vector(first, second, third))
      tx <- first.getTransaction(txid)
      _ = assert(tx.confirmations > 0)
      rawTx <- second.getRawTransaction(txid)
      _ = assert(rawTx.confirmations.exists(_ > 0))
      firstBlock <- first.getBestBlockHash
      secondBlock <- second.getBestBlockHash
    } yield assert(firstBlock == secondBlock)
  }

  it should "ble able to generate blocks with multiple clients and sync inbetween" in {
    val blocksToGenerate = 10

    for {
      (first, second, third) <- clientsF
      allClients = Vector(first, second, third)
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

  it should "be able to wait for disconnected nodes" in {
    for {
      (first, second) <- BitcoindRpcTestUtil.createUnconnectedNodePair(
        clientAccum)
      _ <- first.addNode(second.instance.uri, AddNodeArgument.Add)
      _ <- BitcoindRpcTestUtil.awaitConnection(first, second)

      peerInfo <- first.getPeerInfo
      _ = assert(peerInfo.length == 1)
      _ = assert(peerInfo.head.addnode)
      _ = assert(peerInfo.head.networkInfo.addr == second.instance.uri)

      _ <- first.disconnectNode(peerInfo.head.networkInfo.addr)
      _ <- BitcoindRpcTestUtil.awaitDisconnected(first, second)
      newPeerInfo <- first.getPeerInfo
    } yield assert(newPeerInfo.isEmpty)
  }

  it should "be able to find outputs of previous transactions" in {
    for {
      (first, second, _) <- clientsF
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
