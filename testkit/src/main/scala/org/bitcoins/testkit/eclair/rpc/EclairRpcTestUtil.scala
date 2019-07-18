package org.bitcoins.testkit.eclair.rpc

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
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.api.EclairApi
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.eclair.rpc.json.{PaymentId, PaymentStatus}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.rpc.{BitcoindRpcTestUtil, TestRpcUtil}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import org.bitcoins.rpc.config.BitcoindAuthCredentials

/**
  * @define nodeLinkDoc
  * Creates two Eclair nodes that are connected in the following manner:
  * {{{
  *   node1 <-> node2 <-> node3 <-> node4
  * }}}
  *
  * Each double sided arrow represents a P2P connection as well as a funded
  * channel
  *
  */
trait EclairRpcTestUtil extends BitcoinSLogger {
  import collection.JavaConverters._

  def randomDirName: String =
    0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  def randomEclairDatadir(): File = new File(s"/tmp/${randomDirName}/.eclair/")

  def cannonicalDatadir = new File(s"${System.getenv("HOME")}/.reg_eclair/")

  lazy val network = RegTest

  /**
    * Makes a best effort to get a 0.16 bitcoind instance
    */
  def startedBitcoindRpcClient(instance: BitcoindInstance = bitcoindInstance())(
      implicit actorSystem: ActorSystem): Future[BitcoindRpcClient] = {
    BitcoindRpcTestUtil.startedBitcoindRpcClient(instance)
  }

  def bitcoindInstance(
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort,
      zmqPort: Int = RpcUtil.randomPort): BitcoindInstance = {
    BitcoindRpcTestUtil.v17Instance(port = port,
                                    rpcPort = rpcPort,
                                    zmqPort = zmqPort)
  }

  //cribbed from https://github.com/Christewart/eclair/blob/bad02e2c0e8bd039336998d318a861736edfa0ad/eclair-core/src/test/scala/fr/acinq/eclair/integration/IntegrationSpec.scala#L140-L153
  private def commonConfig(
      bitcoindInstance: BitcoindInstance,
      port: Int = RpcUtil.randomPort,
      apiPort: Int = RpcUtil.randomPort): Config = {
    val configMap = {
      Map(
        "eclair.chain" -> "regtest",
        "eclair.spv" -> false,
        "eclair.server.public-ips.1" -> "127.0.0.1",
        "eclair.server.binding-ip" -> "0.0.0.0",
        "eclair.server.port" -> port,
        "eclair.bitcoind.rpcuser" -> bitcoindInstance.authCredentials
          .asInstanceOf[BitcoindAuthCredentials.PasswordBased]
          .username,
        "eclair.bitcoind.rpcpassword" -> bitcoindInstance.authCredentials
          .asInstanceOf[BitcoindAuthCredentials.PasswordBased]
          .password,
        "eclair.bitcoind.rpcport" -> bitcoindInstance.rpcUri.getPort,
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
        "eclair.max-payment-fee" -> 10, // avoid complaints about too high fees
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
      implicit system: ActorSystem): Future[EclairRpcClient] = {
    import system.dispatcher
    val bitcoindRpcF: Future[BitcoindRpcClient] = {
      if (bitcoindRpcOpt.isDefined) {
        Future.successful(bitcoindRpcOpt.get)
      } else {
        EclairRpcTestUtil.startedBitcoindRpcClient()
      }
    }

    val randInstanceF = bitcoindRpcF.map(randomEclairInstance(_))
    val eclairRpcF = randInstanceF.map(i => new EclairRpcClient(i))

    val startedF = eclairRpcF.flatMap(_.start())

    startedF.flatMap(_ => eclairRpcF)
  }

  def cannonicalEclairClient()(
      implicit system: ActorSystem): EclairRpcClient = {
    val inst = cannonicalEclairInstance()
    new EclairRpcClient(inst)
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
    * is in the [[org.bitcoins.core.protocol.ln.channel.ChannelState ChannelState.NORMAL]]
    * for this [[org.bitcoins.eclair.rpc.client.EclairRpcClient EclairRpcClient]]
    * @param client
    * @param chanId
    */
  def awaitUntilChannelNormal(client: EclairApi, chanId: ChannelId)(
      implicit system: ActorSystem): Future[Unit] = {
    awaitUntilChannelState(client, chanId, ChannelState.NORMAL)
  }

  def awaitUntilChannelClosing(client: EclairApi, chanId: ChannelId)(
      implicit system: ActorSystem): Future[Unit] = {
    awaitUntilChannelState(client, chanId, ChannelState.CLOSING)
  }

  private def awaitUntilChannelState(
      client: EclairApi,
      chanId: ChannelId,
      state: ChannelState)(implicit system: ActorSystem): Future[Unit] = {
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

    TestAsyncUtil.retryUntilSatisfiedF(conditionF = () => isState(),
                                       duration = 1.seconds)
  }

  def awaitUntilPaymentSucceeded(
      client: EclairApi,
      paymentId: PaymentId,
      duration: FiniteDuration = 1.second,
      maxTries: Int = 50,
      failFast: Boolean = true)(implicit system: ActorSystem): Future[Unit] = {
    awaitUntilPaymentStatus(client,
                            paymentId,
                            PaymentStatus.SUCCEEDED,
                            duration,
                            maxTries,
                            failFast)
  }

  def awaitUntilPaymentFailed(
      client: EclairApi,
      paymentId: PaymentId,
      duration: FiniteDuration = 1.second,
      maxTries: Int = 50,
      failFast: Boolean = false)(implicit system: ActorSystem): Future[Unit] = {
    awaitUntilPaymentStatus(client,
                            paymentId,
                            PaymentStatus.FAILED,
                            duration,
                            maxTries,
                            failFast)
  }

  def awaitUntilPaymentPending(
      client: EclairApi,
      paymentId: PaymentId,
      duration: FiniteDuration = 1.second,
      maxTries: Int = 50,
      failFast: Boolean = true)(implicit system: ActorSystem): Future[Unit] = {
    awaitUntilPaymentStatus(client,
                            paymentId,
                            PaymentStatus.PENDING,
                            duration,
                            maxTries,
                            failFast)
  }

  private def awaitUntilPaymentStatus(
      client: EclairApi,
      paymentId: PaymentId,
      state: PaymentStatus,
      duration: FiniteDuration,
      maxTries: Int,
      failFast: Boolean)(implicit system: ActorSystem): Future[Unit] = {
    logger.debug(s"Awaiting payment ${paymentId} to enter ${state} state")

    def isState(): Future[Boolean] = {

      val sentInfoF = client.getSentInfo(paymentId)
      sentInfoF.map { payment =>
        if (failFast && payment.exists(_.status == PaymentStatus.FAILED)) {
          throw new RuntimeException(s"Payment ${paymentId} has failed")
        }
        if (!payment.exists(_.status == state)) {
          logger.trace(
            s"Payment ${paymentId} has not entered ${state} yet. Currently in ${payment.map(_.status).mkString(",")}")
          false
        } else {
          true
        }
      }(system.dispatcher)
    }

    TestAsyncUtil.retryUntilSatisfiedF(conditionF = () => isState(),
                                       duration = duration,
                                       maxTries = maxTries)
  }

  private def createNodeLink(
      bitcoindRpcClient: Option[BitcoindRpcClient],
      channelAmount: MilliSatoshis)(
      implicit actorSystem: ActorSystem): Future[EclairNodes4] = {
    implicit val ec: ExecutionContext = actorSystem.dispatcher
    val internalBitcoindF = {
      if (bitcoindRpcClient.isDefined) {
        Future.successful(bitcoindRpcClient.get)
      } else {
        EclairRpcTestUtil.startedBitcoindRpcClient()
      }
    }

    val pair1: Future[(EclairRpcClient, EclairRpcClient)] = {
      internalBitcoindF.flatMap(b => createNodePair(Some(b)))
    }

    val pair2: Future[(EclairRpcClient, EclairRpcClient)] = {
      internalBitcoindF.flatMap(b => createNodePair(Some(b)))
    }

    def open(
        c1: EclairRpcClient,
        c2: EclairRpcClient): Future[FundedChannelId] = {
      openChannel(n1 = c1,
                  n2 = c2,
                  amt = channelAmount.toSatoshis,
                  pushMSat = MilliSatoshis(channelAmount.toLong / 2))
    }

    val nodeVecF: Future[Vector[EclairRpcClient]] = {
      pair1.flatMap {
        case (first, second) =>
          pair2.flatMap {
            case (third, fourth) =>
              // we need to make sure the second and third nodes are connected
              val connected = EclairRpcTestUtil.connectLNNodes(second, third)
              connected.map { _ =>
                Vector(first, second, third, fourth)
              }

          }
      }
    }

    val openChannelsFNested: Future[List[Future[FundedChannelId]]] = {
      nodeVecF.map { nodeVec =>
        List(open(nodeVec.head, nodeVec(1)),
             open(nodeVec(1), nodeVec(2)),
             open(nodeVec(2), nodeVec(3)))

      }
    }

    val openChannelsF: Future[List[FundedChannelId]] = {
      openChannelsFNested.flatMap(Future.sequence(_))
    }

    val genBlocksF = openChannelsF.flatMap { _ =>
      internalBitcoindF.flatMap(_.generate(3))
    }

    genBlocksF.flatMap { _ =>
      nodeVecF.map { nodeVec =>
        EclairNodes4(nodeVec.head, nodeVec(1), nodeVec(2), nodeVec(3))
      }
    }
  }

  /**
    * $nodeLinkDoc
    * @note Blocks the current thread
    * @return A 4-tuple of the created nodes' respective
    *         [[org.bitcoins.eclair.rpc.client.EclairRpcClient EclairRpcClient]]
    */
  def createNodeLink(
      bitcoindRpcClient: BitcoindRpcClient
  )(implicit actorSystem: ActorSystem): Future[EclairNodes4] = {
    createNodeLink(Some(bitcoindRpcClient), DEFAULT_CHANNEL_MSAT_AMT)
  }

  /**
    * $nodeLinkDoc
    * @note Blocks the current thread
    * @return A 4-tuple of the created nodes' respective
    *         [[org.bitcoins.eclair.rpc.client.EclairRpcClient EclairRpcClient]]
    */
  def createNodeLink(
      bitcoindRpcClient: BitcoindRpcClient,
      channelAmount: MilliSatoshis)(
      implicit actorSystem: ActorSystem): Future[EclairNodes4] = {
    createNodeLink(Some(bitcoindRpcClient), channelAmount)
  }

  /**
    * $nodeLinkDoc
    * @note Blocks the current thread
    * @return A 4-tuple of the created nodes' respective
    *         [[org.bitcoins.eclair.rpc.client.EclairRpcClient EclairRpcClient]]
    */
  def createNodeLink()(
      implicit actorSystem: ActorSystem): Future[EclairNodes4] = {
    createNodeLink(None, DEFAULT_CHANNEL_MSAT_AMT)
  }

  /**
    * $nodeLinkDoc
    * @note Blocks the current thread
    * @return A 4-tuple of the created nodes' respective
    *         [[org.bitcoins.eclair.rpc.client.EclairRpcClient EclairRpcClient]]
    */
  def createNodeLink(
      channelAmount: MilliSatoshis
  )(implicit actorSystem: ActorSystem): Future[EclairNodes4] = {
    createNodeLink(None, channelAmount)
  }

  /**
    * Creates two Eclair nodes that are connected together and returns their
    * respective [[org.bitcoins.eclair.rpc.client.EclairRpcClient EclairRpcClient]]s
    */
  def createNodePair(bitcoindRpcClientOpt: Option[BitcoindRpcClient])(
      implicit system: ActorSystem): Future[
    (EclairRpcClient, EclairRpcClient)] = {
    import system.dispatcher
    val bitcoindRpcClientF: Future[BitcoindRpcClient] = {

      if (bitcoindRpcClientOpt.isDefined) {
        Future.successful(bitcoindRpcClientOpt.get)
      } else {
        EclairRpcTestUtil.startedBitcoindRpcClient()
      }
    }

    val e1InstanceF =
      bitcoindRpcClientF.map(EclairRpcTestUtil.eclairInstance(_))
    val e2InstanceF =
      bitcoindRpcClientF.map(EclairRpcTestUtil.eclairInstance(_))

    val clientF = e1InstanceF.flatMap { e1 =>
      val e = new EclairRpcClient(e1)
      logger.debug(
        s"Temp eclair directory created ${e.getDaemon.authCredentials.datadir}")
      e.start().map(_ => e)
    }
    val otherClientF = e2InstanceF.flatMap { e2 =>
      val e = new EclairRpcClient(e2)
      logger.debug(
        s"Temp eclair directory created ${e.getDaemon.authCredentials.datadir}")
      e.start().map(_ => e)
    }

    val connectedLnF: Future[(EclairRpcClient, EclairRpcClient)] =
      clientF.flatMap { c1 =>
        otherClientF.flatMap { c2 =>
          val connectedF = connectLNNodes(c1, c2)
          connectedF.map { _ =>
            (c1, c2)
          }
        }
      }

    connectedLnF
  }

  def connectLNNodes(client: EclairApi, otherClient: EclairApi)(
      implicit
      system: ActorSystem): Future[Unit] = {
    implicit val dispatcher = system.dispatcher
    val infoF = otherClient.getInfo
    val nodeIdF = infoF.map(_.nodeId)
    val connection: Future[Unit] = infoF.flatMap { info =>
      val Array(host, port) = info.publicAddresses.head.split(":")
      client.connect(info.nodeId, host, port.toInt)
    }

    def isConnected(): Future[Boolean] = {

      nodeIdF.flatMap { nodeId =>
        connection.flatMap { _ =>
          val connected: Future[Boolean] = client.isConnected(nodeId)
          connected
        }
      }
    }

    logger.debug(s"Awaiting connection between clients")
    val connected = TestRpcUtil.retryUntilSatisfiedF(conditionF =
                                                       () => isConnected(),
                                                     duration = 1.second)

    connected.map(_ => logger.debug(s"Successfully connected two clients"))

    connected

  }

  /**
    * Sends `numPayments` between `c1` and `c2`. No aspect of the payment
    * (size, description, etc) should be assumed to have a certain value,
    * this method is just for populating channel update history with
    * <i>something<i/>.
    */
  def sendPayments(c1: EclairApi, c2: EclairApi, numPayments: Int = 5)(
      implicit ec: ExecutionContext): Future[Vector[PaymentId]] = {
    val payments = (1 to numPayments)
      .map(MilliSatoshis(_))
      .map(
        sats =>
          c1.createInvoice(s"this is a note for $sats")
            .flatMap(invoice => c2.payInvoice(invoice, sats))
      )

    val resultF = Future.sequence(payments).map(_.toVector)

    resultF.onComplete {
      case Success(_) =>
      case Failure(_) =>
        val nodeId1F = c1.nodeId()
        val nodeId2F = c2.nodeId()
        nodeId1F.flatMap { nid1 =>
          nodeId2F.map { nid2 =>
            logger.error(
              s"Failed to send payments from ${nid1.hex} -> ${nid2.hex}")
          }
        }
    }

    resultF
  }

  private val DEFAULT_CHANNEL_MSAT_AMT = MilliSatoshis(500000000L)

  /** Opens a channel from n1 -> n2 */
  def openChannel(
      n1: EclairRpcClient,
      n2: EclairRpcClient,
      amt: CurrencyUnit = DEFAULT_CHANNEL_MSAT_AMT.toSatoshis,
      pushMSat: MilliSatoshis = MilliSatoshis(
        DEFAULT_CHANNEL_MSAT_AMT.toLong / 2))(
      implicit system: ActorSystem): Future[FundedChannelId] = {

    val bitcoindRpcClient = getBitcoindRpc(n1)
    implicit val ec = system.dispatcher

    val n1NodeIdF = n1.nodeId()
    val n2NodeIdF = n2.nodeId()

    val nodeIdsF: Future[(NodeId, NodeId)] = {
      n1NodeIdF.flatMap(n1 => n2NodeIdF.map(n2 => (n1, n2)))
    }

    val fundedChannelIdF: Future[FundedChannelId] = {
      nodeIdsF.flatMap {
        case (nodeId1, nodeId2) =>
          logger.debug(
            s"Opening a channel from ${nodeId1} -> ${nodeId2} with amount ${amt}")
          n1.open(nodeId = nodeId2,
                  fundingSatoshis = amt,
                  pushMsat = Some(pushMSat),
                  feerateSatPerByte = None,
                  channelFlags = None)
      }
    }

    val gen = fundedChannelIdF.flatMap(_ => bitcoindRpcClient.generate(6))

    val openedF = {
      gen.flatMap { _ =>
        fundedChannelIdF.flatMap { fcid =>
          val chanOpenF = awaitChannelOpened(n1, fcid)
          chanOpenF.map(_ => fcid)
        }
      }
    }

    openedF.flatMap {
      case _ =>
        nodeIdsF.map {
          case (nodeId1, nodeId2) =>
            logger.debug(
              s"Channel successfully opened ${nodeId1} -> ${nodeId2} with amount $amt")
        }
    }

    openedF
  }

  def awaitChannelOpened(client1: EclairApi, chanId: ChannelId)(
      implicit system: ActorSystem): Future[Unit] = {
    EclairRpcTestUtil.awaitUntilChannelNormal(client1, chanId)
  }

  def getBitcoindRpc(eclairRpcClient: EclairRpcClient)(
      implicit system: ActorSystem): BitcoindRpcClient = {
    val bitcoindRpc = {
      val instance = eclairRpcClient.instance
      val auth = instance.authCredentials
      val bitcoindInstance = BitcoindInstance(
        network = instance.network,
        uri = new URI("http://localhost:18333"),
        rpcUri = auth.bitcoindRpcUri,
        authCredentials = auth.bitcoinAuthOpt.get
      )
      new BitcoindRpcClient(bitcoindInstance)(system)
    }
    bitcoindRpc
  }

  /** Shuts down an eclair daemon and the bitcoind daemon it is associated with
    *
    * */
  def shutdown(eclairRpcClient: EclairRpcClient)(
      implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    val bitcoindRpc = getBitcoindRpc(eclairRpcClient)

    logger.debug(s"shutting down eclair")
    val killEclairOpt = eclairRpcClient.stop()
    val killBitcoindF = BitcoindRpcTestUtil.stopServer(bitcoindRpc)

    for {
      _ <- killBitcoindF
    } yield {
      killEclairOpt match {
        case Some(_) =>
          logger.debug(
            "Successfully shutdown eclair and it's corresponding bitcoind")
        case None =>
          logger.info(
            s"Killed a bitcoind instance, but could not find an eclair process to kill")
      }
    }
  }
}

object EclairRpcTestUtil extends EclairRpcTestUtil

case class EclairNodes4(
    c1: EclairRpcClient,
    c2: EclairRpcClient,
    c3: EclairRpcClient,
    c4: EclairRpcClient)
