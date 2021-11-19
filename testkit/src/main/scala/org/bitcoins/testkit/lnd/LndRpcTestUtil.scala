package org.bitcoins.testkit.lnd

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.lnd.rpc.LndRpcClient
import org.bitcoins.lnd.rpc.config.{LndInstanceLocal, LndInstanceRemote}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{
  BitcoindAuthCredentials,
  BitcoindInstance,
  BitcoindInstanceLocal,
  ZmqConfig
}
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{FileUtil, TestkitBinaries}

import java.io.{File, PrintWriter}
import java.net.{InetSocketAddress, URI}
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
  def startedBitcoindRpcClient(
      instanceOpt: Option[BitcoindInstanceLocal] = None)(implicit
      actorSystem: ActorSystem): Future[BitcoindRpcClient] = {
    //need to do something with the Vector.newBuilder presumably?
    BitcoindRpcTestUtil.startedBitcoindRpcClient(instanceOpt, Vector.newBuilder)
  }

  /** Creates a bitcoind instance with the given parameters */
  def bitcoindInstance(
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort,
      zmqConfig: ZmqConfig = RpcUtil.zmqConfig,
      bitcoindV: BitcoindVersion = BitcoindVersion.V21)(implicit
      system: ActorSystem): BitcoindInstanceLocal = {
    BitcoindRpcTestUtil.getInstance(bitcoindVersion = bitcoindV,
                                    port = port,
                                    rpcPort = rpcPort,
                                    zmqConfig = zmqConfig)
  }

  def commonConfig(
      bitcoindInstance: BitcoindInstance,
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort): String = {
    val rawTx = bitcoindInstance.zmqConfig.rawTx.get
    val rawBlock = bitcoindInstance.zmqConfig.rawBlock.get
    s"""
       |noseedbackup = true
       |bitcoin.active = true
       |bitcoin.regtest = true
       |bitcoin.node = bitcoind
       |norest=true
       |debuglevel=critical
       |listen=127.0.0.1:$port
       |rpclisten=127.0.0.1:$rpcPort
       |externalip=127.0.0.1
       |maxpendingchannels=10
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

  def lndInstance(bitcoindRpc: BitcoindRpcClient)(implicit
      system: ActorSystem): LndInstanceLocal = {
    val datadir = lndDataDir(bitcoindRpc, isCannonical = false)
    lndInstance(datadir)
  }

  def lndInstance(datadir: File)(implicit
      system: ActorSystem): LndInstanceLocal = {
    LndInstanceLocal.fromDataDir(datadir)
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

  def connectLNNodes(client: LndRpcClient, otherClient: LndRpcClient)(implicit
      ec: ExecutionContext): Future[Unit] = {
    val infoF = otherClient.getInfo
    val nodeIdF = client.getInfo.map(_.identityPubkey)
    val connectionF: Future[Unit] = infoF.flatMap { info =>
      val uriF: Future[URI] = otherClient.instance match {
        case local: LndInstanceLocal => Future.successful(local.listenBinding)
        case _: LndInstanceRemote =>
          otherClient.getInfo.map(info => new URI(info.uris.head))
      }
      uriF.flatMap(uri =>
        client.connectPeer(NodeId(info.identityPubkey),
                           new InetSocketAddress(uri.getHost, uri.getPort)))
    }

    def isConnected: Future[Boolean] = {
      for {
        nodeId <- nodeIdF
        _ <- connectionF
        res <- otherClient.isConnected(NodeId(nodeId))
      } yield res
    }

    logger.debug(s"Awaiting connection between clients")
    val connected = TestAsyncUtil.retryUntilSatisfiedF(conditionF =
                                                         () => isConnected,
                                                       interval = 1.second)

    connected.map(_ => logger.debug(s"Successfully connected two clients"))

    connected
  }

  def fundLNNodes(
      bitcoind: BitcoindRpcClient,
      client: LndRpcClient,
      otherClient: LndRpcClient)(implicit
      ec: ExecutionContext): Future[Unit] = {
    for {
      addrA <- client.getNewAddress
      addrB <- otherClient.getNewAddress

      _ <- bitcoind.sendMany(Map(addrA -> Bitcoins(1), addrB -> Bitcoins(1)))
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
    } yield ()
  }

  /** Creates two Lnd nodes that are connected together and returns their
    * respective [[org.bitcoins.lnd.rpc.LndRpcClient LndRpcClient]]s
    */
  def createNodePair(bitcoind: BitcoindRpcClient)(implicit
      ec: ExecutionContext): Future[(LndRpcClient, LndRpcClient)] = {

    val actorSystemA =
      ActorSystem.create("bitcoin-s-lnd-test-" + FileUtil.randomDirName)
    val clientA = LndRpcTestClient
      .fromSbtDownload(Some(bitcoind))(actorSystemA)

    val actorSystemB =
      ActorSystem.create("bitcoin-s-lnd-test-" + FileUtil.randomDirName)
    val clientB = LndRpcTestClient
      .fromSbtDownload(Some(bitcoind))(actorSystemB)

    val clientsF = for {
      a <- clientA.start()
      b <- clientB.start()
    } yield (a, b)

    def isSynced: Future[Boolean] = for {
      (client, otherClient) <- clientsF

      infoA <- client.getInfo
      infoB <- otherClient.getInfo
    } yield infoA.syncedToChain && infoB.syncedToChain

    def isFunded: Future[Boolean] = for {
      (client, otherClient) <- clientsF

      balA <- client.walletBalance()
      balB <- otherClient.walletBalance()
    } yield balA.confirmedBalance > Satoshis.zero && balB.confirmedBalance > Satoshis.zero

    for {
      (client, otherClient) <- clientsF

      _ <- connectLNNodes(client, otherClient)
      _ <- fundLNNodes(bitcoind, client, otherClient)

      _ <- AsyncUtil.awaitConditionF(() => isSynced)
      _ <- AsyncUtil.awaitConditionF(() => isFunded)

      _ <- openChannel(bitcoind, client, otherClient)
    } yield (client, otherClient)
  }

  private val DEFAULT_CHANNEL_AMT = Satoshis(500000L)

  /** Opens a channel from n1 -> n2 */
  def openChannel(
      bitcoind: BitcoindRpcClient,
      n1: LndRpcClient,
      n2: LndRpcClient,
      amt: CurrencyUnit = DEFAULT_CHANNEL_AMT,
      pushAmt: CurrencyUnit = DEFAULT_CHANNEL_AMT / Satoshis(2))(implicit
      ec: ExecutionContext): Future[TransactionOutPoint] = {

    val n1NodeIdF = n1.nodeId
    val n2NodeIdF = n2.nodeId

    val nodeIdsF: Future[(NodeId, NodeId)] = {
      n1NodeIdF.flatMap(n1 => n2NodeIdF.map(n2 => (n1, n2)))
    }

    val fundedChannelIdF =
      nodeIdsF.flatMap { case (nodeId1, nodeId2) =>
        logger.debug(
          s"Opening a channel from $nodeId1 -> $nodeId2 with amount $amt")
        n1.openChannel(nodeId = nodeId2,
                       fundingAmount = amt,
                       pushAmt = pushAmt,
                       satPerVByte = SatoshisPerVirtualByte.fromLong(10),
                       privateChannel = false)
          .map(_.get)
      }

    val genF = for {
      _ <- fundedChannelIdF
      address <- bitcoind.getNewAddress
      blocks <- bitcoind.generateToAddress(6, address)
    } yield blocks

    val openedF = {
      genF.flatMap { _ =>
        fundedChannelIdF.flatMap { id =>
          for {
            _ <- awaitUntilChannelActive(n1, id)
            _ <- awaitUntilChannelActive(n2, id)
          } yield id
        }
      }
    }

    openedF.flatMap { _ =>
      nodeIdsF.map { case (nodeId1, nodeId2) =>
        logger.debug(
          s"Channel successfully opened $nodeId1 -> $nodeId2 with amount $amt")
      }
    }

    openedF
  }

  private def awaitUntilChannelActive(
      client: LndRpcClient,
      outPoint: TransactionOutPoint)(implicit
      ec: ExecutionContext): Future[Unit] = {
    def isActive: Future[Boolean] = {
      client.findChannel(outPoint).map {
        case None          => false
        case Some(channel) => channel.active
      }
    }

    TestAsyncUtil.retryUntilSatisfiedF(conditionF = () => isActive,
                                       interval = 1.seconds)
  }
}

object LndRpcTestUtil extends LndRpcTestUtil
