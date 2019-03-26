package org.bitcoins.rpc
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.io.Source

class BitcoindInstanceTest extends BitcoindRpcTest {

  private val source =
    Source.fromURL(getClass.getResource("/sample-bitcoin.conf"))

  private val sampleConf: Seq[String] = {
    source.getLines.toSeq
  }

  private val datadir: Path = Files.createTempDirectory(null)

  override protected def beforeAll(): Unit = {
    val confFile = new File(datadir.toString + "/bitcoin.conf")
    val pw = new PrintWriter(confFile)
    sampleConf.foreach(line => pw.write(line + "\n"))
    pw.close()
  }

  behavior of "BitcoindInstance"

  it should "parse a bitcoin.conf file, start bitcoind, mine some blocks and quit" in {
    val instance = BitcoindInstance.fromDatadir(datadir.toFile)
    val client = new BitcoindRpcClient(instance)

    for {
      _ <- client.start()
      _ <- client.generate(101)
      balance <- client.getBalance
      _ <- BitcoindRpcTestUtil.stopServers(Vector(client))
    } yield assert(balance > Bitcoins(0))

  }

}
