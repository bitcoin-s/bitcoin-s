package org.bitcoins.testkit.util

import java.nio.file.Files

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil

import scala.collection.mutable

abstract class BitcoindRpcTest extends BitcoinSAsyncTest {

  private val dirExists = Files.exists(BitcoindRpcTestUtil.binaryDirectory)

  private val hasContents = dirExists && Files
    .list(BitcoindRpcTestUtil.binaryDirectory)
    .toArray()
    .nonEmpty

  if (!hasContents) {
    import System.err.{println => printerr}
    printerr()
    printerr(s"Run 'sbt downloadBitcoind' to fetch needed binaries")
    sys.error {
      val msg =
        s""""bitcoind binary directory (${BitcoindRpcTestUtil.binaryDirectory}) is empty. 
           |Run 'sbt downloadBitcoind' to fetch needed binaries""".stripMargin
      msg
    }
  }

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

  override def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clientAccum.result)
    super.afterAll
  }
}
