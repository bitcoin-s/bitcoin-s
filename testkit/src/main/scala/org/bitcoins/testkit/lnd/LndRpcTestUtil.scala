package org.bitcoins.testkit.lnd

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.core.protocol.ln.channel.FundedChannelId
import org.bitcoins.lnd.rpc.LndRpcClient
import org.bitcoins.lnd.rpc.config.LndInstance
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstance}
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{FileUtil, TestkitBinaries}

import java.io.{File, PrintWriter}
import java.nio.file.Path
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Properties

trait LndRpcTestUtil extends Logging {

  val sbtBinaryDirectory: Path =
    TestkitBinaries.baseBinaryDirectory.resolve("lnd")

  def randomLndDatadir(): File =
    new File(s"/tmp/lnd-test/${FileUtil.randomDirName}/.lnd/")

  def cannonicalDatadir = new File(s"${Properties.userHome}/.reg_lnd/")

  /** Makes a best effort to get a 0.21 bitcoind instance
    */
  def startedBitcoindRpcClient(instance: BitcoindInstance = bitcoindInstance())(
      implicit actorSystem: ActorSystem): Future[BitcoindRpcClient] = {
    //need to do something with the Vector.newBuilder presumably?
    BitcoindRpcTestUtil.startedBitcoindRpcClient(instance, Vector.newBuilder)
  }

  /** Creates a bitcoind instance with the given parameters */
  def bitcoindInstance(
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort,
      zmqPort: Int = RpcUtil.randomPort,
      bitcoindV: BitcoindVersion = BitcoindVersion.V21): BitcoindInstance = {
    bitcoindV match {
      case BitcoindVersion.V21 =>
        BitcoindRpcTestUtil.v21Instance(port = port,
                                        rpcPort = rpcPort,
                                        zmqPort = zmqPort)
      case BitcoindVersion.V20 =>
        BitcoindRpcTestUtil.v20Instance(port = port,
                                        rpcPort = rpcPort,
                                        zmqPort = zmqPort)
      case BitcoindVersion.V19 =>
        BitcoindRpcTestUtil.v19Instance(port = port,
                                        rpcPort = rpcPort,
                                        zmqPort = zmqPort)
      case BitcoindVersion.V18 =>
        BitcoindRpcTestUtil.v18Instance(port = port,
                                        rpcPort = rpcPort,
                                        zmqPort = zmqPort)
      case BitcoindVersion.V17 =>
        BitcoindRpcTestUtil.v17Instance(port = port,
                                        rpcPort = rpcPort,
                                        zmqPort = zmqPort)
      case BitcoindVersion.V16 =>
        BitcoindRpcTestUtil.v16Instance(port = port,
                                        rpcPort = rpcPort,
                                        zmqPort = zmqPort)
      case BitcoindVersion.Experimental =>
        BitcoindRpcTestUtil.vExperimentalInstance(port = port,
                                                  rpcPort = rpcPort,
                                                  zmqPort = zmqPort)
      case BitcoindVersion.Unknown =>
        sys.error(s"Cannot start lnd with an unknown instance of bitcoind")
    }
  }

  def commonConfig(
      bitcoindInstance: BitcoindInstance,
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort): String = {
    val rawTx = bitcoindInstance.zmqConfig.rawTx.get
    val rawBlock = bitcoindInstance.zmqConfig.rawBlock.get
    s"""
       |bitcoin.active = true
       |bitcoin.regtest = true
       |bitcoin.node = bitcoind
       |norest=true
       |debuglevel=critical
       |listen=127.0.0.1:$port
       |rpclisten=127.0.0.1:$rpcPort
       |bitcoind.rpcuser = ${bitcoindInstance.authCredentials
      .asInstanceOf[BitcoindAuthCredentials.PasswordBased]
      .username}
       |bitcoind.rpcpass = ${bitcoindInstance.authCredentials
      .asInstanceOf[BitcoindAuthCredentials.PasswordBased]
      .password}
       |bitcoind.rpchost = 127.0.0.1:${bitcoindInstance.rpcUri.getPort}
       |bitcoind.zmqpubrawtx = tcp://127.0.0.1:${rawTx.getPort}
       |bitcoind.zmqpubrawblock = tcp://127.0.0.1:${rawBlock.getPort}
       |""".stripMargin
  }

  def lndDataDir(
      bitcoindRpcClient: BitcoindRpcClient,
      isCannonical: Boolean): File = {
    val bitcoindInstance = bitcoindRpcClient.instance
    if (isCannonical) {
      //assumes that the ${HOME}/.lnd/lnd.conf file is created AND a bitcoind instance is running
      cannonicalDatadir
    } else {
      //creates a random lnd datadir, but still assumes that a bitcoind instance is running right now
      val datadir = randomLndDatadir()
      datadir.mkdirs()
      logger.trace(s"Creating temp lnd dir ${datadir.getAbsolutePath}")

      val config = commonConfig(bitcoindInstance)

      new PrintWriter(new File(datadir, "lnd.conf")) {
        write(config)
        close()
      }
      datadir
    }
  }

  def lndInstance(bitcoindRpc: BitcoindRpcClient): LndInstance = {
    val datadir = lndDataDir(bitcoindRpc, isCannonical = false)
    lndInstance(datadir)
  }

  def lndInstance(datadir: File): LndInstance = {
    LndInstance.fromDataDir(datadir)
  }

  /** Returns a `Future` that is completed when both lnd and bitcoind have the same block height
    * Fails the future if they are not synchronized within the given timeout.
    */
  def awaitLndInSync(lnd: LndRpcClient, bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    TestAsyncUtil.retryUntilSatisfiedF(conditionF =
                                         () => clientInSync(lnd, bitcoind),
                                       interval = 1.seconds)
  }

  private def clientInSync(client: LndRpcClient, bitcoind: BitcoindRpcClient)(
      implicit ec: ExecutionContext): Future[Boolean] =
    for {
      blockCount <- bitcoind.getBlockCount
      info <- client.getInfo
    } yield info.blockHeight == blockCount

  /** Shuts down an lnd daemon and the bitcoind daemon it is associated with
    */
  def shutdown(lndRpcClient: LndRpcClient)(implicit
      system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    val shutdownF = for {
      bitcoindRpc <- startedBitcoindRpcClient()
      _ <- BitcoindRpcTestUtil.stopServer(bitcoindRpc)
      _ <- lndRpcClient.stop()
    } yield {
      logger.debug("Successfully shutdown lnd and it's corresponding bitcoind")
    }
    shutdownF.failed.foreach { err: Throwable =>
      logger.info(
        s"Killed a bitcoind instance, but could not find an lnd process to kill")
      throw err
    }
    shutdownF
  }

  case class LndNetwork(
      bitcoind: BitcoindRpcClient,
      testLndNode: LndRpcClient,
      networkLndNodes: Vector[LndRpcClient],
      channelIds: Vector[FundedChannelId]) {

    def shutdown()(implicit ec: ExecutionContext): Future[Unit] =
      for {
        _ <- Future.sequence(networkLndNodes.map(_.stop()))
        _ <- testLndNode.stop()
        _ <- bitcoind.stop()
      } yield ()
  }
}

object LndRpcTestUtil extends LndRpcTestUtil
