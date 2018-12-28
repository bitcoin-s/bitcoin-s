package org.bitcoins.eclair.rpc

import java.io.{File, PrintWriter}
import java.net.URI

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.ln.channel.{
  ChannelId,
  ChannelState,
  FundedChannelId
}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindInstance, ZmqConfig}
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.util.RpcUtil

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

trait EclairTestUtil extends BitcoinSLogger {
  import collection.JavaConverters._

  def randomDirName: String =
    0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  def randomEclairDatadir(): File = new File(s"/tmp/${randomDirName}/.eclair/")

  def cannonicalDatadir = new File(s"${System.getenv("HOME")}/.reg_eclair/")

  lazy val network = RegTest

  def bitcoindInstance(
      port: Int = randomPort,
      rpcPort: Int = randomPort,
      zmqPort: Int = randomPort): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val auth = BitcoindRpcTestUtil.authCredentials(uri, rpcUri, zmqPort, false)

    BitcoindInstance(network = network,
                     uri = uri,
                     rpcUri = rpcUri,
                     authCredentials = auth,
                     zmqConfig = ZmqConfig.fromPort(zmqPort))
  }

  //cribbed from https://github.com/Christewart/eclair/blob/bad02e2c0e8bd039336998d318a861736edfa0ad/eclair-core/src/test/scala/fr/acinq/eclair/integration/IntegrationSpec.scala#L140-L153
  private def commonConfig(
      bitcoindInstance: BitcoindInstance,
      port: Int = randomPort,
      apiPort: Int = randomPort): Config = {
    val configMap = {
      Map(
        "eclair.chain" -> "regtest",
        "eclair.spv" -> false,
        "eclair.server.public-ips.1" -> "127.0.0.1",
        "eclair.server.binding-ip" -> "0.0.0.0",
        "eclair.server.port" -> port,
        "eclair.bitcoind.rpcuser" -> bitcoindInstance.authCredentials.username,
        "eclair.bitcoind.rpcpassword" -> bitcoindInstance.authCredentials.password,
        "eclair.bitcoind.rpcport" -> bitcoindInstance.authCredentials.rpcPort,
        // newer versions of Eclair has removed this config setting, in favor of
        // the below it. All three are included here for good measure
        "eclair.bitcoind.zmq" -> bitcoindInstance.zmqConfig.rawTx.get.toString,
        "eclair.bitcoind.zmqblock" -> bitcoindInstance.zmqConfig.rawBlock.get.toString,
        "eclair.bitcoind.zmqtx" -> bitcoindInstance.zmqConfig.rawTx.get.toString,
        "eclair.api.enabled" -> true,
        "eclair.api.binding-ip" -> "127.0.0.1",
        "eclair.api.password" -> "abc123",
        "eclair.api.port" -> apiPort,
        "eclair.mindepth-blocks" -> 2,
        "eclair.max-htlc-value-in-flight-msat" -> 100000000000L,
        "eclair.router-broadcast-interval" -> "2 second",
        "eclair.auto-reconnect" -> false,
        "eclair.db.driver" -> "org.sqlite.JDBC",
        "eclair.db.regtest.url" -> "jdbc:sqlite:regtest/",
        "eclair.alias" -> "suredbits"
      )
    }
    val c = ConfigFactory.parseMap(configMap.asJava)
    c
  }

  def eclairDataDir(
      bitcoindRpcClient: BitcoindRpcClient,
      isCannonical: Boolean): File = {
    val bitcoindInstance = bitcoindRpcClient.instance
    if (isCannonical) {
      //assumes that the ${HOME}/.eclair/eclair.conf file is created AND a bitcoind instance is running
      cannonicalDatadir
    } else {
      //creates a random eclair datadir, but still assumes that a bitcoind instance is running right now
      val datadir = randomEclairDatadir
      datadir.mkdirs()
      logger.trace(s"Creating temp eclair dir ${datadir.getAbsolutePath}")

      val config = commonConfig(bitcoindInstance)

      new PrintWriter(new File(datadir, "eclair.conf")) {
        write(config.root().render())
        close
      }
      datadir
    }
  }

  /** Assumes bitcoind is running already and you have specified correct bindings in eclair.conf */
  def cannonicalEclairInstance(): EclairInstance = {
    val datadir = cannonicalDatadir
    eclairInstance(datadir)
  }

  def eclairInstance(datadir: File): EclairInstance = {
    val instance = EclairInstance.fromDatadir(datadir)
    instance
  }

  /** Starts the given bitcoind instance and then starts the eclair instance */
  def eclairInstance(bitcoindRpc: BitcoindRpcClient): EclairInstance = {
    val datadir = eclairDataDir(bitcoindRpc, false)
    eclairInstance(datadir)
  }

  def randomEclairInstance(bitcoindRpc: BitcoindRpcClient): EclairInstance = {
    val datadir = eclairDataDir(bitcoindRpc, false)
    eclairInstance(datadir)
  }

  def randomEclairClient(bitcoindRpcOpt: Option[BitcoindRpcClient] = None)(
      implicit system: ActorSystem): EclairRpcClient = {
    val bitcoindRpc = {
      if (bitcoindRpcOpt.isDefined) {
        bitcoindRpcOpt.get
      } else {
        BitcoindRpcTestUtil.startedBitcoindRpcClient()
      }
    }

    val randInstance = randomEclairInstance(bitcoindRpc)
    val eclairRpc = new EclairRpcClient(randInstance)
    eclairRpc.start()

    RpcUtil.awaitCondition(() => eclairRpc.isStarted(), duration = 1.seconds)

    eclairRpc
  }

  def cannonicalEclairClient()(
      implicit system: ActorSystem): EclairRpcClient = {
    val inst = cannonicalEclairInstance()
    new EclairRpcClient(inst)
  }

  def randomPort: Int = {
    val firstAttempt = Math.abs(scala.util.Random.nextInt % 15000)
    if (firstAttempt < network.port) {
      firstAttempt + network.port
    } else firstAttempt
  }

  def deleteTmpDir(dir: File): Boolean = {
    if (!dir.isDirectory) {
      dir.delete()
    } else {
      dir.listFiles().foreach(deleteTmpDir)
      dir.delete()
    }
  }

  /**
    * Doesn't return until the given channelId
    * is in the [[ChannelState.NORMAL]] for this [[EclairRpcClient]]
    * @param client
    * @param chanId
    */
  def awaitUntilChannelNormal(client: EclairRpcClient, chanId: ChannelId)(
      implicit system: ActorSystem): Unit = {
    awaitUntilChannelState(client, chanId, ChannelState.NORMAL)
  }

  def awaitUntilChannelClosing(client: EclairRpcClient, chanId: ChannelId)(
      implicit system: ActorSystem): Unit = {
    awaitUntilChannelState(client, chanId, ChannelState.CLOSING)
  }

  private def awaitUntilChannelState(
      client: EclairRpcClient,
      chanId: ChannelId,
      state: ChannelState)(implicit system: ActorSystem): Unit = {
    logger.debug(s"Awaiting ${chanId} to enter ${state} state")
    def isState(): Future[Boolean] = {
      val chanF = client.channel(chanId)
      chanF.map { chan =>
        if (!(chan.state == state)) {
          logger.trace(
            s"ChanId ${chanId} has not entered ${state} yet. Currently in ${chan.state}")
        }
        chan.state == state
      }(system.dispatcher)
    }

    RpcUtil.awaitConditionF(conditionF = () => isState(), duration = 1.seconds)

    logger.debug(s"${chanId} has successfully entered the ${state} state")
    ()
  }

  /**
    * Creates two eclair nodes that are connected together and returns their
    * respective [[EclairRpcClient]]s
    */
  def createNodePair(bitcoindRpcClientOpt: Option[BitcoindRpcClient])(
      implicit system: ActorSystem): (EclairRpcClient, EclairRpcClient) = {
    val bitcoindRpcClient = {
      bitcoindRpcClientOpt.getOrElse(
        BitcoindRpcTestUtil.startedBitcoindRpcClient())
    }

    val e1Instance = EclairTestUtil.eclairInstance(bitcoindRpcClient)
    val e2Instance = EclairTestUtil.eclairInstance(bitcoindRpcClient)

    val client = new EclairRpcClient(e1Instance)
    val otherClient = new EclairRpcClient(e2Instance)

    logger.debug(
      s"Temp eclair directory created ${client.getDaemon.authCredentials.datadir}")
    logger.debug(
      s"Temp eclair directory created ${otherClient.getDaemon.authCredentials.datadir}")

    client.start()
    otherClient.start()

    RpcUtil.awaitCondition(condition = () => client.isStarted(),
                           duration = 1.second)

    RpcUtil.awaitCondition(condition = () => otherClient.isStarted(),
                           duration = 1.second)

    logger.debug(s"Both clients started")

    connectLNNodes(client, otherClient)

    (client, otherClient)
  }

  def connectLNNodes(client: EclairRpcClient, otherClient: EclairRpcClient)(
      implicit
      system: ActorSystem): Unit = {
    implicit val dispatcher = system.dispatcher
    val infoF = otherClient.getInfo

    val connection: Future[String] = infoF.flatMap { info =>
      client.connect(info.nodeId, "localhost", info.port)
    }

    def isConnected(): Future[Boolean] = {
      val nodeIdF = infoF.map(_.nodeId)
      nodeIdF.flatMap { nodeId =>
        connection.flatMap { _ =>
          val connected: Future[Boolean] = client.isConnected(nodeId)
          connected
        }
      }
    }

    logger.debug(s"Awaiting connection between clients")
    RpcUtil.awaitConditionF(conditionF = () => isConnected(),
                            duration = 1.second)
    logger.debug(s"Successfully connected two clients")

    ()
  }

  /** Opens a channel from n1 -> n2 */
  def openChannel(
      n1: EclairRpcClient,
      n2: EclairRpcClient,
      amt: CurrencyUnit,
      pushMSat: MilliSatoshis)(
      implicit system: ActorSystem): Future[FundedChannelId] = {

    val bitcoindRpcClient = getBitcoindRpc(n1)
    implicit val ec = system.dispatcher
    val fundedChannelIdF: Future[FundedChannelId] = {
      n2.nodeId.flatMap { nodeId =>
        n1.getInfo.map { info =>
          logger.debug(
            s"Opening a channel from ${info.nodeId} -> ${nodeId} with amount ${amt}")

        }
        n1.open(nodeId = nodeId,
                fundingSatoshis = amt,
                pushMsat = Some(pushMSat),
                feerateSatPerByte = None,
                channelFlags = None)
      }
    }
    val gen = fundedChannelIdF.flatMap(_ => bitcoindRpcClient.generate(6))

    val opened = {
      gen.flatMap { _ =>
        fundedChannelIdF.map { fcid =>
          awaitChannelOpened(n1, fcid)
          fcid
        }
      }
    }
    opened.map {
      case _ =>
        logger.debug(
          s"Channel successfully opened ${n1.getNodeURI} -> ${n2.getNodeURI} with amount $amt")
    }
    opened
  }

  def awaitChannelOpened(client1: EclairRpcClient, chanId: ChannelId)(
      implicit system: ActorSystem): Unit = {
    EclairTestUtil.awaitUntilChannelNormal(client1, chanId)
  }

  def getBitcoindRpc(eclairRpcClient: EclairRpcClient)(
      implicit system: ActorSystem): BitcoindRpcClient = {
    val bitcoindRpc = {
      val eclairAuth = eclairRpcClient.instance.authCredentials
      val bitcoindRpcPort = eclairAuth.bitcoinRpcPort.get

      val bitcoindInstance = BitcoindInstance(
        network = eclairRpcClient.instance.network,
        uri = new URI("http://localhost:18333"),
        rpcUri = new URI(s"http://localhost:${bitcoindRpcPort}"),
        authCredentials =
          eclairRpcClient.instance.authCredentials.bitcoinAuthOpt.get,
      )
      new BitcoindRpcClient(bitcoindInstance)
    }
    bitcoindRpc
  }

  /** Shuts down an eclair daemon and the bitcoind daemon it is associated with */
  def shutdown(eclairRpcClient: EclairRpcClient)(
      implicit system: ActorSystem): Unit = {
    val bitcoindRpc = getBitcoindRpc(eclairRpcClient)

    eclairRpcClient.stop()

    bitcoindRpc.stop()

    RpcUtil.awaitServerShutdown(bitcoindRpc)
  }
}

object EclairTestUtil extends EclairTestUtil
