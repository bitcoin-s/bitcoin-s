package org.bitcoins.eclair.rpc

import java.io.{ File, PrintWriter }
import java.net.URI

import akka.actor.ActorSystem
import org.bitcoins.core.config.{ NetworkParameters, RegTest }
import org.bitcoins.core.protocol.ln.channel.{ ChannelId, ChannelState }
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.{ EclairAuthCredentials, EclairInstance }
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.rpc.config.{ BitcoindAuthCredentials, BitcoindInstance }
import org.bitcoins.rpc.{ BitcoindRpcTestUtil, RpcUtil }
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

trait EclairRpcTestUtil {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  def randomDirName: String = {
    BitcoindRpcTestUtil.randomDirName
  }

  /**
   * Creates a datadir and places the username/password combo
   * in the bitcoin.conf in the datadir
   */
  def bitcoindAuthCredentials(
    uri: URI,
    rpcUri: URI,
    zmqPort: Int): BitcoindAuthCredentials = {
    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    val conf = new java.io.File(f.getAbsolutePath + "/bitcoin.conf")
    conf.createNewFile()
    val username = "random_user_name"
    val pass = randomDirName
    val pw = new PrintWriter(conf)
    pw.write("rpcuser=" + username + "\n")
    pw.write("rpcpassword=" + pass + "\n")
    pw.write("rpcport=" + rpcUri.getPort + "\n")
    pw.write("port=" + uri.getPort + "\n")
    pw.write("daemon=1\n")
    pw.write("server=1\n")
    pw.write("debug=1\n")
    pw.write("regtest=1\n")
    pw.write("walletbroadcast=0\n")

    pw.write(s"zmqpubhashtx=tcp://127.0.0.1:${zmqPort}\n")
    pw.write(s"zmqpubhashblock=tcp://127.0.0.1:${zmqPort}\n")
    pw.write(s"zmqpubrawtx=tcp://127.0.0.1:${zmqPort}\n")
    pw.write(s"zmqpubrawblock=tcp://127.0.0.1:${zmqPort}\n")

    pw.close()
    BitcoindAuthCredentials(username, pass, f)
  }

  lazy val network: NetworkParameters = {
    BitcoindRpcTestUtil.network
  }

  def bitcoindInstance(
    port: Int = randomPort,
    rpcPort: Int = randomPort,
    zmqPort: Int = randomPort): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val auth = bitcoindAuthCredentials(uri, rpcUri, zmqPort)

    BitcoindInstance(network, uri, rpcUri, auth, Some(zmqPort))
  }

  def startedBitcoindInstance()(implicit system: ActorSystem): BitcoindInstance = {

    val i = bitcoindInstance()

    //start the bitcoind instance so eclair can properly use it
    val rpc = new BitcoindRpcClient(i)(system)
    rpc.start()

    logger.debug(s"Starting bitcoind at ${i.authCredentials.datadir}")
    RpcUtil.awaitServer(rpc)

    //fund the wallet by generating 102 blocks, need this to get over coinbase maturity
    val blockGen = rpc.generate(102)
    i
  }

  /**
   * Creates a datadir and places the username/password combo
   * in the eclair.conf in the datadir
   */
  def eclairAuthCredentials(uri: URI, rpcUri: URI, bitcoind: BitcoindInstance): EclairAuthCredentials = {
    val bitcoindAuth = bitcoind.authCredentials
    val bitcoindRpcUri = bitcoind.rpcUri

    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    val conf = new java.io.File(f.getAbsolutePath + "/eclair.conf")
    conf.createNewFile()
    val username = "random_user_name"
    val pass = randomDirName

    val bitcoindUser = bitcoindAuth.username
    val bitcoindPass = bitcoindAuth.password

    val zmqUri: String = s"tcp://127.0.0.1:${bitcoind.zmqPortOpt.get}"
    val pw = new PrintWriter(conf)
    pw.println("eclair.chain=regtest")

    pw.println(s"eclair.bitcoind.rpcuser=${bitcoindUser}")
    pw.println("eclair.bitcoind.rpcpassword=\"" + bitcoindPass + "\"")
    pw.println(s"eclair.bitcoind.rpcport=${bitcoindRpcUri.getPort}")
    pw.println(s"eclair.bitcoind.host=${bitcoindRpcUri.getHost}")
    pw.println("eclair.bitcoind.zmq =\"" + zmqUri + "\"")

    pw.println("eclair.api.enabled=true")
    pw.println("eclair.node.alias = \"" + username + "\"")
    pw.println("eclair.server.port = " + uri.getPort)
    pw.println("eclair.api.password = \"" + pass + "\"")
    pw.println("eclair.api.port = " + rpcUri.getPort)
    pw.println("eclair.api.binding-ip = \"127.0.0.1\"")
    pw.close()

    EclairAuthCredentials(username, pass, bitcoindUser, bitcoindPass, f)
  }

  lazy val bitcoinNetwork = RegTest

  def eclairInstance(bitcoind: BitcoindInstance, port: Int = randomPort, rpcPort: Int = randomPort)(implicit system: ActorSystem): EclairInstance = {
    val uri = new URI("http://127.0.0.1:" + port)
    val rpcUri = new URI("http://127.0.0.1:" + rpcPort)
    val auth = eclairAuthCredentials(uri, rpcUri, bitcoind)
    EclairInstance(bitcoinNetwork, uri, rpcUri, auth)
  }

  def randomPort: Int = {
    BitcoindRpcTestUtil.randomPort
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
  def awaitUntilChannelNormal(client: EclairRpcClient, chanId: ChannelId)(implicit system: ActorSystem): Unit = {
    awaitUntilChannelState(client, chanId, ChannelState.NORMAL)
  }

  def awaitUntilChannelClosing(client: EclairRpcClient, chanId: ChannelId)(implicit system: ActorSystem): Unit = {
    awaitUntilChannelState(client, chanId, ChannelState.CLOSING)
  }

  private def awaitUntilChannelState(client: EclairRpcClient, chanId: ChannelId, state: ChannelState)(implicit system: ActorSystem): Unit = {

    def isState(): Future[Boolean] = {
      val chanF = client.channel(chanId)
      chanF.map { chan =>
        chan.state == state
      }(system.dispatcher)
    }

    RpcUtil.awaitConditionF(
      conditionF = isState)
  }

  /**
   * Creates two eclair nodes that are connected together and returns their
   * respective [[EclairRpcClient]]s
   */
  def createNodePair(bitcoindInstance: BitcoindInstance)(implicit system: ActorSystem): (EclairRpcClient, EclairRpcClient) = {

    implicit val ec = system.dispatcher
    val e1Instance = EclairRpcTestUtil.eclairInstance(bitcoindInstance)
    val e2Instance = EclairRpcTestUtil.eclairInstance(bitcoindInstance)

    val client = new EclairRpcClient(e1Instance)
    val otherClient = new EclairRpcClient(e2Instance)

    logger.info(s"Temp eclair directory created ${client.getDaemon.authCredentials.datadir}")
    logger.info(s"Temp eclair directory created ${otherClient.getDaemon.authCredentials.datadir}")

    client.start()
    otherClient.start()

    RpcUtil.awaitCondition(
      condition = client.isStarted,
      duration = 1.second)

    RpcUtil.awaitCondition(
      condition = otherClient.isStarted,
      duration = 1.second)

    logger.debug(s"Both clients started")

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
    RpcUtil.awaitConditionF(
      conditionF = isConnected,
      duration = 1.second)

    (client, otherClient)

  }
}

object EclairRpcTestUtil extends EclairRpcTestUtil