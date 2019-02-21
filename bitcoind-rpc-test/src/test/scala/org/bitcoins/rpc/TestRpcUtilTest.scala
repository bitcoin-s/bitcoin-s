package org.bitcoins.rpc

import java.io.File

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.testkit.rpc.{BitcoindRpcTestUtil, TestRpcUtil}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Success

class TestRpcUtilTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system = ActorSystem("RpcUtilTest_ActorSystem")
  implicit val ec = system.dispatcher

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
    boolLaterDoneAnd(true, trueLater)
  }

  behavior of "TestRpcUtil"

  it should "complete immediately if condition is true" in {
    TestRpcUtil
      .retryUntilSatisfiedF(conditionF = () => Future.successful(true),
                            duration = 0.millis)
      .map { _ =>
        succeed
      }
  }

  it should "fail if condition is false" in {
    recoverToSucceededIf[TestFailedException] {
      TestRpcUtil.retryUntilSatisfiedF(conditionF =
                                         () => Future.successful(false),
                                       duration = 0.millis)
    }
  }

  it should "succeed after a delay" in {
    val boolLater = trueLater(delay = 250)
    TestRpcUtil.retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater)).map { _ =>
      succeed
    }
  }

  it should "fail if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    recoverToSucceededIf[TestFailedException] {
      TestRpcUtil.retryUntilSatisfiedF(boolLaterDoneAndTrue(boolLater),
                                       duration = 0.millis)
    }
  }

  it should "succeed immediately if condition is true" in {
    TestRpcUtil.awaitCondition(condition = () => true, 0.millis)
    succeed
  }

  it should "timeout if condition is false" in {
    assertThrows[TestFailedException] {
      TestRpcUtil.awaitCondition(condition = () => false, duration = 0.millis)
    }
  }

  it should "block for a delay and then succeed" in {
    val boolLater = trueLater(delay = 250)
    val before: Long = System.currentTimeMillis
    TestRpcUtil.awaitConditionF(boolLaterDoneAndTrue(boolLater))
    val after: Long = System.currentTimeMillis
    assert(after - before >= 250)
  }

  it should "timeout if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    assertThrows[TestFailedException] {
      TestRpcUtil.awaitConditionF(boolLaterDoneAndTrue(boolLater),
                                  duration = 0.millis)
    }
  }

  "BitcoindRpcUtil" should "create a temp bitcoin directory when creating a DaemonInstance, and then delete it" in {
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
    implicit val m: ActorMaterializer = ActorMaterializer.create(system)
    implicit val ec = m.executionContext

    val instance = BitcoindRpcTestUtil.instance()
    val client = new BitcoindRpcClient(instance)
    val startedF = client.start()

    startedF.map { _ =>
      client.stop()
      succeed
    }
  }

  it should "be able to create a connected node pair with 100 blocks and then delete them" in {
    BitcoindRpcTestUtil.createNodePair().flatMap {
      case (client1, client2) =>
        assert(client1.getDaemon.authCredentials.datadir.isDirectory)
        assert(client2.getDaemon.authCredentials.datadir.isDirectory)

        client1.getAddedNodeInfo(client2.getDaemon.uri).flatMap { nodes =>
          assert(nodes.nonEmpty)

          client1.getBlockCount.flatMap { count1 =>
            assert(count1 == 100)

            client2.getBlockCount.map { count2 =>
              assert(count2 == 100)

              BitcoindRpcTestUtil.deleteNodePair(client1, client2)
              assert(!client1.getDaemon.authCredentials.datadir.exists)
              assert(!client2.getDaemon.authCredentials.datadir.exists)
            }
          }
        }
    }
  }

  override def afterAll(): Unit = {
    Await.result(system.terminate(), 10.seconds)
  }
}
