package org.bitcoins.testkit.util

import grizzled.slf4j.Logging

import java.nio.file.Files
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil

import scala.collection.mutable
import scala.concurrent.{Await, Future}

abstract class BitcoindRpcTest extends BitcoinSAsyncTest with Logging {

  private val dirExists =
    Files.exists(BitcoindRpcTestClient.sbtBinaryDirectory)

  private val hasContents = dirExists && Files
    .list(BitcoindRpcTestClient.sbtBinaryDirectory)
    .toArray()
    .nonEmpty

  if (!hasContents) {
    import System.err.{println => printerr}
    printerr()
    printerr(s"Run 'sbt downloadBitcoind' to fetch needed binaries")
    sys.error {
      val msg =
        s""""bitcoind binary directory (${BitcoindRpcTestClient.sbtBinaryDirectory}) is empty. 
           |Run 'sbt downloadBitcoind' to fetch needed binaries""".stripMargin
      msg
    }
  }

  /** Bitcoind RPC clients can be added to this builder
    * as they are created in tests. After tests have
    * stopped running (either by succeeding or failing)
    * all clients found in the builder is shut down.
    */
  lazy val clientAccum: mutable.Builder[
    BitcoindRpcClient,
    Vector[BitcoindRpcClient]] = Vector.newBuilder

  override def afterAll(): Unit = {
    val stopF = BitcoindRpcTestUtil.stopServers(clientAccum.result())
    Await.result(stopF, duration)
    super.afterAll()
  }

  def startClient(client: BitcoindRpcClient): Future[Unit] = {
    BitcoindRpcTestUtil.startServers(Vector(client))
  }
}
