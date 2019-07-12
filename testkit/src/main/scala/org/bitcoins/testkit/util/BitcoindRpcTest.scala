package org.bitcoins.testkit.util

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

abstract class BitcoindRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /** AsyncFlatSpec provides its own execution context, but for some
    * reason that doesn't allow doing anything in `afterAll`. We add
    * our own.
    */
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

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
    val stopF = BitcoindRpcTestUtil.stopServers(clientAccum.result)
    Await.result(stopF, 10.seconds)
  }
}
