package org.bitcoins.rpc

import java.io.{File, PrintWriter}
import java.net.URI

import akka.actor.ActorSystem
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstance}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

trait BitcoindRpcTestUtil extends BitcoinSLogger {

  lazy val network: NetworkParameters = RegTest

  def randomDirName: String =
    0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /**
   * Creates a datadir and places the username/password combo
   * in the bitcoin.conf in the datadir
   */
  def authCredentials(
    uri: URI,
    rpcUri: URI,
    zmqPort: Int,
    pruneMode: Boolean): BitcoindAuthCredentials = {
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

    if (pruneMode) {
      logger.info(s"Creating pruned node for ${f.getAbsolutePath}")
      pw.write("prune=1\n")
    }

    pw.close()
    BitcoindAuthCredentials(username, pass, rpcUri.getPort, f)
  }

  def instance(
    port: Int = randomPort,
    rpcPort: Int = randomPort,
    zmqPort: Int = randomPort,
    pruneMode: Boolean = false): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val auth = authCredentials(uri, rpcUri, zmqPort, pruneMode)
    val instance = BitcoindInstance(
      network = network,
      uri = uri,
      rpcUri = rpcUri,
      authCredentials = auth,
      zmqPortOpt = Some(zmqPort))

    instance
  }

  def randomPort: Int = {
    val firstAttempt = Math.abs(scala.util.Random.nextInt % 15000)
    if (firstAttempt < network.port) {
      firstAttempt + network.port
    } else firstAttempt
  }

  /**
   * Starts the given servers, and waits for them (in a blocking manner)
   * until they are started.
   */
  def startServers(servers: BitcoindRpcClient*)(implicit system: ActorSystem): Unit = {
    servers.foreach(_.start())
    servers.foreach(RpcUtil.awaitServer(_))
  }

  /**
    * Stops the given servers, and waits for them (in a blocking manner)
    * until they are stopped. Deletes their temp dir.
    */
  def stopServers(servers: BitcoindRpcClient*)(implicit system: ActorSystem): Unit =
    servers.foreach(server => {
      server.stop()
      RpcUtil.awaitServerShutdown(server)
      deleteTmpDir(server.getDaemon.authCredentials.datadir)
    })

  def deleteTmpDir(dir: File): Boolean = {
    if (!dir.isDirectory) {
      dir.delete()
    } else {
      dir.listFiles().foreach(deleteTmpDir)
      dir.delete()
    }
  }

  def awaitConnection(
    from: BitcoindRpcClient,
    to: BitcoindRpcClient,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher

    def isConnected(): Future[Boolean] = {
      from.getAddedNodeInfo(to.getDaemon.uri)
        .map { info =>
          info.nonEmpty && info.head.connected.contains(true)
        }
    }

    RpcUtil.awaitConditionF(
      conditionF = isConnected,
      duration = duration,
      maxTries = maxTries)
  }

  def awaitSynced(
    client1: BitcoindRpcClient,
    client2: BitcoindRpcClient,
    duration: FiniteDuration = 1.second,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher

    def isSynced(): Future[Boolean] = {
      client1.getBestBlockHash.flatMap { hash1 =>
        client2.getBestBlockHash.map { hash2 =>
          hash1 == hash2
        }
      }
    }

    RpcUtil.awaitConditionF(
      conditionF = isSynced,
      duration = duration,
      maxTries = maxTries)
  }

  def awaitSameBlockHeight(
    client1: BitcoindRpcClient,
    client2: BitcoindRpcClient,
    duration: FiniteDuration = 1.second,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher

    def isSameBlockHeight(): Future[Boolean] = {
      client1.getBlockCount.flatMap { count1 =>
        client2.getBlockCount.map { count2 =>
          count1 == count2
        }
      }
    }

    RpcUtil.awaitConditionF(
      conditionF = isSameBlockHeight,
      duration = duration,
      maxTries = maxTries)
  }

  def awaitDisconnected(
    from: BitcoindRpcClient,
    to: BitcoindRpcClient,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher

    def isDisconnected(): Future[Boolean] = {
      val f = from.getAddedNodeInfo(to.getDaemon.uri)
        .map(info => info.isEmpty || info.head.connected.contains(false))

      f

    }

    RpcUtil.awaitConditionF(
      conditionF = isDisconnected,
      duration = duration,
      maxTries = maxTries)
  }


  def hasSeenBlock(client1: BitcoindRpcClient, hash: DoubleSha256Digest)(implicit ec: ExecutionContext): Future[Boolean] = {
    val p = Promise[Boolean]()

    client1.getBlock(hash).onComplete {
      case Success(_) => p.success(true)
      case Failure(_) => p.success(false)
    }

    p.future
  }

  def startedBitcoindRpcClient(instance: BitcoindInstance = BitcoindRpcTestUtil.instance())(implicit system: ActorSystem): BitcoindRpcClient = {
    implicit val ec: ExecutionContext = system.dispatcher
    //start the bitcoind instance so eclair can properly use it
    val rpc = new BitcoindRpcClient(instance)(system)
    rpc.start()

    logger.debug(s"Starting bitcoind at ${instance.authCredentials.datadir}")
    RpcUtil.awaitServer(rpc)

    val blocksToGenerate = 102
    //fund the wallet by generating 102 blocks, need this to get over coinbase maturity
    val blockGen = rpc.generate(blocksToGenerate)

    def isBlocksGenerated(): Future[Boolean] = {
      rpc.getBlockCount.map(_ >= blocksToGenerate)
    }

    RpcUtil.awaitConditionF(isBlocksGenerated)

    rpc
  }

  def deleteNodePair(client1: BitcoindRpcClient, client2: BitcoindRpcClient)(implicit actorSystem: ActorSystem): Unit = stopServers(client1, client2)

}

object BitcoindRpcTestUtil extends BitcoindRpcTestUtil
