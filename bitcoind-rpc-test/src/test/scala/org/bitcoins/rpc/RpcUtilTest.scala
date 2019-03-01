package org.bitcoins.rpc

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

class RpcUtilTest extends AsyncFlatSpec with BeforeAndAfterAll {

  private val clients = Vector.newBuilder[BitcoindRpcClient]

  private lazy val clientsF = BitcoindRpcTestUtil.createNodePair().map {
    case (first, second) =>
      clients += (first, second)
      (first, second)
  }

  implicit val system: ActorSystem = ActorSystem("RpcUtilTest_ActorSystem")
  implicit val ec: ExecutionContext = system.dispatcher

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

  behavior of "RpcUtil"

  it should "complete immediately if condition is true" in {
    RpcUtil
      .retryUntilSatisfiedF(conditionF = () => Future.successful(true),
                            duration = 0.millis)
      .map { _ =>
        succeed
      }
  }

  it should "fail if condition is false" in {
    recoverToSucceededIf[TestFailedException] {
      RpcUtil.retryUntilSatisfiedF(conditionF = () => Future.successful(false),
                                   duration = 0.millis)
    }
  }

  it should "succeed after a delay" in {
    val boolLater = trueLater(delay = 250)
    RpcUtil.retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater)).map { _ =>
      succeed
    }
  }

  it should "fail if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    recoverToSucceededIf[TestFailedException] {
      RpcUtil.retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater),
                                   duration = 0.millis)
    }
  }

  it should "succeed immediately if condition is true" in {
    RpcUtil.awaitCondition(condition = () => true, 0.millis)
    succeed
  }

  it should "timeout if condition is false" in {
    assertThrows[TestFailedException] {
      RpcUtil.awaitCondition(condition = () => false, duration = 0.millis)
    }
  }

  it should "block for a delay and then succeed" in {
    val boolLater = trueLater(delay = 250)
    val before: Long = System.currentTimeMillis
    RpcUtil.awaitConditionF(boolLaterDoneAndTrue(boolLater))
    val after: Long = System.currentTimeMillis
    assert(after - before >= 250)
  }

  it should "timeout if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    assertThrows[TestFailedException] {
      RpcUtil.awaitConditionF(boolLaterDoneAndTrue(boolLater),
                              duration = 0.millis)
    }
  }

  behavior of "BitcoindRpcUtil"

  it should "create a temp bitcoin directory when creating a DaemonInstance, and then delete it" in {
    val instance = BitcoindRpcTestUtil.instance(BitcoindRpcTestUtil.randomPort,
                                                BitcoindRpcTestUtil.randomPort)
    val dir = instance.authCredentials.datadir
    assert(dir.isDirectory)
    assert(
      dir.listFiles.contains(new File(dir.getAbsolutePath + "/bitcoin.conf")))
    BitcoindRpcTestUtil.deleteTmpDir(dir)
    assert(!dir.exists)
  }

  it should "be able to create a single node, wait for it to start and then delete it" in {
    val instance = BitcoindRpcTestUtil.instance()
    val client = new BitcoindRpcClient(instance)
    val startedF = client.start()

    startedF.map { _ =>
      client.stop()
      succeed
    }
  }

  it should "be able to create a connected node pair with 101 blocks and then delete them" in {
    BitcoindRpcTestUtil.createNodePair().flatMap {
      case (client1, client2) =>
        assert(client1.getDaemon.authCredentials.datadir.isDirectory)
        assert(client2.getDaemon.authCredentials.datadir.isDirectory)

        client1.getAddedNodeInfo(client2.getDaemon.uri).flatMap { nodes =>
          assert(nodes.nonEmpty)

          client1.getBlockCount.flatMap { count1 =>
            assert(count1 == 101)

            client2.getBlockCount.map { count2 =>
              assert(count2 == 101)

              BitcoindRpcTestUtil.deleteNodePair(client1, client2)
              assert(!client1.getDaemon.authCredentials.datadir.exists)
              assert(!client2.getDaemon.authCredentials.datadir.exists)
            }
          }
        }
    }
  }

  it should "be able to generate and sync blocks" in {
    for {
      (first, second) <- clientsF
      _ = clients += (first, second)
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
    val blocksToGenerate = 10

    for {
      (first, second) <- clientsF
      heightPreGeneration <- first.getBlockCount
      _ <- BitcoindRpcTestUtil.generateAllAndSync(Vector(first, second),
                                                  blocks = blocksToGenerate)
      firstHash <- first.getBestBlockHash
      secondHash <- second.getBestBlockHash
      heightPostGeneration <- first.getBlockCount
    } yield {
      assert(firstHash == secondHash)
      assert(heightPostGeneration - heightPreGeneration == blocksToGenerate * 2)
    }
  }

  it should "be able to find outputs of previous transactions" in {
    for {
      (first, second) <- clientsF
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

  override def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(
      clients.result()
    )

    TestKit.shutdownActorSystem(system)
  }
}
