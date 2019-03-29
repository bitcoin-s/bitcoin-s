package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

abstract class BitcoindRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  implicit val system: ActorSystem =
    ActorSystem(getClass.getSimpleName, BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  /**
    * Bitcoind RPC clients can be added to this builder
    * as they are created in tests. After tests have
    * stopped running (either by succeeding or failing)
    * all clients found in the builder is shut down.
    */
  lazy val clientAccum: mutable.Builder[
    BitcoindRpcClient,
    Vector[BitcoindRpcClient]] = Vector.newBuilder

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clientAccum.result)
    val _ = Await.result(system.terminate, 10.seconds)
  }
}
