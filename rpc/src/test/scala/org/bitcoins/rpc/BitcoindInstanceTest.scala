package org.bitcoins.rpc
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.util.RpcUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.Future
import scala.io.Source

class BitcoindInstanceTest extends AsyncFlatSpec with BeforeAndAfterAll {

  private implicit val actorSystem: ActorSystem = ActorSystem(
    "BitcoindInstanceTest")

  private val sampleConf: Seq[String] =
    Source.fromResource("sample-bitcoin.conf").mkString.split("\n")
  private val datadir: Path = Files.createTempDirectory(null)

  override protected def beforeAll(): Unit = {
    val confFile = new File(datadir.toString + "/bitcoin.conf")
    val pw = new PrintWriter(confFile)
    sampleConf.foreach(line => pw.write(line + "\n"))
    pw.close()
  }

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(actorSystem)
  }

  behavior of "BitcoindInstance"

  it should "parse a bitcoin.conf file, start bitcoind, mine some blocks and quit" in {
    val instance = BitcoindInstance.fromDatadir(datadir.toFile)
    val client = new BitcoindRpcClient(instance)
    BitcoindRpcTestUtil.startServers(Vector(client))
    RpcUtil.awaitServer(client)

    for {
      _ <- client.generate(101)
      balance <- client.getBalance
      _ <- {
        assert(balance > Bitcoins(0))
        client.stop()
      }
      _ <- Future.successful(RpcUtil.awaitServerShutdown(client))
    } yield succeed

  }

}
