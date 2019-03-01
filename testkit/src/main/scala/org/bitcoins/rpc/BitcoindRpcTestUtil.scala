package org.bitcoins.rpc

import java.io.{File, PrintWriter}
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  RpcOpts
}
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.config.{
  BitcoindAuthCredentials,
  BitcoindInstance,
  ZmqConfig
}
import org.bitcoins.rpc.jsonmodels.{
  GetBlockWithTransactionsResult,
  GetTransactionResult,
  SignRawTransactionResult
}
import org.bitcoins.util.AsyncUtil

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.concurrent._
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util._

//noinspection AccessorLikeMethodIsEmptyParen
trait BitcoindRpcTestUtil extends BitcoinSLogger {
  import scala.collection.JavaConverters._

  @tailrec
  private def randomDirName: String = {
    val dirname = 0.until(5).map(_ => Random.alphanumeric.head).mkString
    val dir = new File(dirname)
    if (!dir.exists()) {
      dirname
    } else {
      randomDirName
    }
  }

  /**
    * Standard config used for testing purposes
    */
  def standardConfig: Config = {
    def newUri: URI = new URI(s"http://localhost:$randomPort")
    config(uri = newUri,
           rpcUri = newUri,
           zmqPort = randomPort,
           pruneMode = false)
  }

  def config(
      uri: URI,
      rpcUri: URI,
      zmqPort: Int,
      pruneMode: Boolean): Config = {
    val pass = randomDirName
    val username = "random_user_name"
    val values = Map[String, String](
      "rpcuser" -> username,
      "rpcpassword" -> pass,
      "rpcport" -> rpcUri.getPort.toString,
      "port" -> uri.getPort.toString,
      "daemon" -> "1",
      "server" -> "1",
      "debug" -> "1",
      "regtest" -> "1",
      "walletbroadcast" -> "1",
      "txindex" -> (if (pruneMode) "0" else "1"), // pruning and txindex are not compatible
      "zmqpubhashtx" -> s"tcp://127.0.0.1:$zmqPort",
      "zmqpubhashblock" -> s"tcp://127.0.0.1:$zmqPort",
      "zmqpubrawtx" -> s"tcp://127.0.0.1:$zmqPort",
      "zmqpubrawblock" -> s"tcp://127.0.0.1:$zmqPort",
      "prune" -> (if (pruneMode) "1" else "0")
    )

    val javaMap = values.asJava
    ConfigFactory.parseMap(javaMap)
  }

  /**
    * Assumes the `config` object has a `datadir` string. Returns the written
    * file.
    */
  def writeConfigToFile(config: Config): File = {

    val confSet = config.entrySet.asScala
    val confStr =
      confSet
        .map(entry => {
          val key = entry.getKey
          val value = entry.getValue.unwrapped
          s"$key=$value"
        })
        .mkString("\n")

    val datadir = new File(config.getString("datadir"))
    datadir.mkdir()

    val confFile = new java.io.File(datadir.getAbsolutePath + "/bitcoin.conf")
    confFile.createNewFile()

    val pw = new PrintWriter(confFile)
    pw.write(confStr)
    pw.close()

    confFile
  }

  /**
    * Creates a datadir and places the username/password combo
    * in the bitcoin.conf in the datadir
    */
  def authCredentials(
      uri: URI,
      rpcUri: URI,
      zmqPort: Int,
      pruneMode: Boolean): BitcoindAuthCredentials = {
    val conf = config(uri, rpcUri, zmqPort, pruneMode)

    val configWithDatadir =
      if (conf.hasPath("datadir")) {
        conf
      } else {
        conf.withValue("datadir",
                       ConfigValueFactory.fromAnyRef("/tmp/" + randomDirName))
      }

    val configFile = writeConfigToFile(configWithDatadir)

    val username = configWithDatadir.getString("rpcuser")
    val pass = configWithDatadir.getString("rpcpassword")

    BitcoindAuthCredentials(username,
                            pass,
                            rpcUri.getPort,
                            configFile.getParentFile)
  }

  lazy val network: RegTest.type = RegTest

  private val V16_ENV = "BITCOIND_V16_PATH"
  private val V17_ENV = "BITCOIND_V17_PATH"

  private def getFileFromEnv(env: String): File =
    new File(
      Properties
        .envOrNone(env)
        .getOrElse(throw new IllegalArgumentException(
          s"$env environment variable is not set")))

  private def getBinary(version: BitcoindVersion): File =
    version match {
      case BitcoindVersion.V16     => getFileFromEnv(V16_ENV)
      case BitcoindVersion.V17     => getFileFromEnv(V17_ENV)
      case BitcoindVersion.Unknown => BitcoindInstance.DEFAULT_BITCOIND_LOCATION
    }

  def instance(
      port: Int = randomPort,
      rpcPort: Int = randomPort,
      zmqPort: Int = randomPort,
      pruneMode: Boolean = false,
      versionOpt: Option[BitcoindVersion] = None): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val auth = authCredentials(uri, rpcUri, zmqPort, pruneMode)
    val binary = versionOpt match {
      case Some(version) =>
        getBinary(version)
      case None => BitcoindInstance.DEFAULT_BITCOIND_LOCATION
    }
    val instance = BitcoindInstance(network = network,
                                    uri = uri,
                                    rpcUri = rpcUri,
                                    authCredentials = auth,
                                    zmqConfig = ZmqConfig.fromPort(zmqPort),
                                    binary = binary)

    instance
  }

  def v16Instance(
      port: Int = randomPort,
      rpcPort: Int = randomPort,
      zmqPort: Int = randomPort,
      pruneMode: Boolean = false,
  ): BitcoindInstance =
    instance(port = port,
             rpcPort = rpcPort,
             zmqPort = zmqPort,
             pruneMode = pruneMode,
             versionOpt = Some(BitcoindVersion.V16))

  def v17Instance(
      port: Int = randomPort,
      rpcPort: Int = randomPort,
      zmqPort: Int = randomPort,
      pruneMode: Boolean = false,
  ): BitcoindInstance =
    instance(port = port,
             rpcPort = rpcPort,
             zmqPort = zmqPort,
             pruneMode = pruneMode,
             versionOpt = Some(BitcoindVersion.V17))

  def randomPort: Int = {
    val firstAttempt = Math.abs(scala.util.Random.nextInt % 15000)
    if (firstAttempt < network.port) {
      firstAttempt + network.port
    } else firstAttempt
  }

  def startServers(servers: Vector[BitcoindRpcClient])(
      implicit ec: ExecutionContext): Future[Unit] = {
    val startedServers = servers.map(_.start())

    Future.sequence(startedServers).map(_ => ())
  }

  def stopServers(servers: Vector[BitcoindRpcClient])(
      implicit system: ActorSystem): Future[Unit] = {
    implicit val ec: ExecutionContextExecutor = system.getDispatcher

    val serverStops = servers.map { s =>
      val stopF = s.stop()
      deleteTmpDir(s.getDaemon.authCredentials.datadir)
      stopF
    }
    Future.sequence(serverStops).map(_ => ())
  }

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
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    def isConnected(): Future[Boolean] = {
      from
        .getAddedNodeInfo(to.getDaemon.uri)
        .map { info =>
          info.nonEmpty && info.head.connected.contains(true)
        }
    }

    AsyncUtil.awaitConditionF(conditionF = () => isConnected(),
                              duration = duration,
                              maxTries = maxTries)
  }

  def awaitSynced(
      client1: BitcoindRpcClient,
      client2: BitcoindRpcClient,
      duration: FiniteDuration = 1.second,
      maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    def isSynced(): Future[Boolean] = {
      client1.getBestBlockHash.flatMap { hash1 =>
        client2.getBestBlockHash.map { hash2 =>
          hash1 == hash2
        }
      }
    }

    AsyncUtil.awaitConditionF(conditionF = () => isSynced(),
                              duration = duration,
                              maxTries = maxTries)
  }

  def awaitSameBlockHeight(
      client1: BitcoindRpcClient,
      client2: BitcoindRpcClient,
      duration: FiniteDuration = 1.second,
      maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    def isSameBlockHeight(): Future[Boolean] = {
      client1.getBlockCount.flatMap { count1 =>
        client2.getBlockCount.map { count2 =>
          count1 == count2
        }
      }
    }

    AsyncUtil.awaitConditionF(conditionF = () => isSameBlockHeight(),
                              duration = duration,
                              maxTries = maxTries)
  }

  def awaitDisconnected(
      from: BitcoindRpcClient,
      to: BitcoindRpcClient,
      duration: FiniteDuration = 100.milliseconds,
      maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    def isDisconnected(): Future[Boolean] = {
      val f = from
        .getAddedNodeInfo(to.getDaemon.uri)
        .map(info => info.isEmpty || info.head.connected.contains(false))

      f

    }

    AsyncUtil.awaitConditionF(conditionF = () => isDisconnected(),
                              duration = duration,
                              maxTries = maxTries)
  }

  /**
    * Returns a pair of unconnected
    * [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]s
    * with no blocks
    */
  def createUnconnectedNodePair(
      port1: Int = randomPort,
      rpcPort1: Int = randomPort,
      port2: Int = randomPort,
      rpcPort2: Int = randomPort)(
      implicit
      system: ActorSystem): Future[(BitcoindRpcClient, BitcoindRpcClient)] = {
    implicit val ec: ExecutionContextExecutor = system.getDispatcher
    val client1: BitcoindRpcClient = new BitcoindRpcClient(
      instance(port1, rpcPort1))
    val client2: BitcoindRpcClient = new BitcoindRpcClient(
      instance(port2, rpcPort2))

    val start1F = client1.start()
    val start2F = client2.start()

    for {
      _ <- start1F
      _ <- start2F
    } yield (client1, client2)

  }

  /**
    * Returns a pair of [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]
    * that are connected with 100 blocks in the chain
    */
  def createNodePair(
      port1: Int = randomPort,
      rpcPort1: Int = randomPort,
      port2: Int = randomPort,
      rpcPort2: Int = randomPort)(implicit system: ActorSystem): Future[
    (BitcoindRpcClient, BitcoindRpcClient)] = {
    implicit val executionContext: ExecutionContext = system.dispatcher
    val clientsF = createUnconnectedNodePair(port1 = port1,
                                             rpcPort1 = rpcPort1,
                                             port2 = port2,
                                             rpcPort2 = rpcPort2)

    def cleanUp(
        attempt: Try[_],
        client1: BitcoindRpcClient,
        client2: BitcoindRpcClient) =
      attempt match {
        case Success(_) => Future.successful(())
        case Failure(exc) =>
          deleteNodePair(client1, client2)
          Future.failed(exc)
      }

    for {
      (client1, client2) <- clientsF
      _ <- client1.addNode(client2.getDaemon.uri, AddNodeArgument.Add)
      connT = Try { awaitConnection(client1, client2) }
      _ <- cleanUp(connT, client1, client2)
      _ <- client1.generate(100)
      syncT = Try { awaitSynced(client1, client2) }
      _ <- cleanUp(syncT, client1, client2)
    } yield (client1, client2)
  }

  def createRawCoinbaseTransaction(
      sender: BitcoindRpcClient,
      receiver: BitcoindRpcClient,
      amount: Bitcoins = Bitcoins(1))(
      implicit executionContext: ExecutionContext): Future[Transaction] = {
    for {
      blocks <- sender.generate(2)
      block0 <- sender.getBlock(blocks(0))
      block1 <- sender.getBlock(blocks(1))
      transaction0 <- sender.getTransaction(block0.tx(0))
      transaction1 <- sender.getTransaction(block1.tx(0))
      input0 = TransactionOutPoint(transaction0.txid.flip,
                                   UInt32(transaction0.blockindex.get))
      input1 = TransactionOutPoint(transaction1.txid.flip,
                                   UInt32(transaction1.blockindex.get))
      sig: ScriptSignature = ScriptSignature.empty
      address <- receiver.getNewAddress
      tx <- sender.createRawTransaction(
        Vector(TransactionInput(input0, sig, UInt32(1)),
               TransactionInput(input1, sig, UInt32(2))),
        Map(address -> amount))
    } yield tx

  }

  /**
    * Bitcoin Core 0.16 and 0.17 has diffrent APIs for signing raw transactions.
    * This method tries to construct either a
    * [[org.bitcoins.rpc.client.v16.BitcoindV16RpcClient BitcoindV16RpcClient]]
    * or a [[org.bitcoins.rpc.client.v16.BitcoindV16RpcClient BitcoindV16RpcClient]]
    * from the provided `signer`, and then calls the appropriate method on the result.
    *
    * Throws a [[RuntimeException]] if no versioned
    * [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]
    * can be constructed.
    */
  def signRawTransaction(
      signer: BitcoindRpcClient,
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] = Vector.empty
  )(implicit actorSystemw: ActorSystem): Future[SignRawTransactionResult] =
    signer match {
      case v17: BitcoindV17RpcClient =>
        v17.signRawTransactionWithWallet(transaction, utxoDeps)
      case v16: BitcoindV16RpcClient =>
        v16.signRawTransaction(transaction, utxoDeps)
      case unknown: BitcoindRpcClient =>
        val v16T = BitcoindV16RpcClient.fromUnknownVersion(unknown)
        val v17T = BitcoindV17RpcClient.fromUnknownVersion(unknown)
        (v16T, v17T) match {
          case (Failure(_), Failure(_)) =>
            throw new RuntimeException(
              "Could not figure out version of provided bitcoind RPC client!")
          case (Success(_), Success(_)) =>
            throw new RuntimeException(
              "This should not happen, managed to construct different versioned RPC clients from one single client")
          case (Success(v16), Failure(_)) =>
            v16.signRawTransaction(transaction, utxoDeps)
          case (Failure(_), Success(v17)) =>
            v17.signRawTransactionWithWallet(transaction, utxoDeps)
        }
    }

  def sendCoinbaseTransaction(
      sender: BitcoindRpcClient,
      receiver: BitcoindRpcClient,
      amount: Bitcoins = Bitcoins(1))(
      implicit actorSystem: ActorSystem): Future[GetTransactionResult] = {
    implicit val materializer: ActorMaterializer =
      ActorMaterializer.create(actorSystem)
    implicit val ec: ExecutionContextExecutor = materializer.executionContext
    createRawCoinbaseTransaction(sender, receiver, amount)
      .flatMap(signRawTransaction(sender, _))
      .flatMap { signedTransaction =>
        sender
          .generate(100)
          .flatMap { _ => // Can't spend coinbase until depth 100
            sender
              .sendRawTransaction(signedTransaction.hex, allowHighFees = true)
              .flatMap { transactionHash =>
                sender.getTransaction(transactionHash)
              }
          }
      }
  }

  def getFirstBlock(
      implicit
      node: BitcoindRpcClient,
      executionContext: ExecutionContext): Future[
    GetBlockWithTransactionsResult] = {
    node
      .getBlockHash(1)
      .flatMap(node.getBlockWithTransactions)
  }

  def fundBlockChainTransaction(
      sender: BitcoindRpcClient,
      address: BitcoinAddress,
      amount: Bitcoins)(
      implicit system: ActorSystem): Future[DoubleSha256Digest] = {
    implicit val mat: ActorMaterializer = ActorMaterializer.create(system)
    implicit val ec: ExecutionContextExecutor = mat.executionContext
    fundMemPoolTransaction(sender, address, amount).flatMap { txid =>
      sender.generate(1).map { _ =>
        txid
      }
    }
  }

  def fundMemPoolTransaction(
      sender: BitcoindRpcClient,
      address: BitcoinAddress,
      amount: Bitcoins)(
      implicit system: ActorSystem): Future[DoubleSha256Digest] = {
    implicit val executionContext: ExecutionContext =
      system.getDispatcher
    sender
      .createRawTransaction(Vector.empty, Map(address -> amount))
      .flatMap(sender.fundRawTransaction)
      .flatMap { fundedTx =>
        signRawTransaction(sender, fundedTx.hex).flatMap { signedTx =>
          sender.sendRawTransaction(signedTx.hex)
        }
      }
  }

  def deleteNodePair(
      client1: BitcoindRpcClient,
      client2: BitcoindRpcClient): Unit = {
    client1.stop()
    client2.stop()
    deleteTmpDir(client1.getDaemon.authCredentials.datadir)
    deleteTmpDir(client2.getDaemon.authCredentials.datadir)
    ()
  }

  def hasSeenBlock(client1: BitcoindRpcClient, hash: DoubleSha256Digest)(
      implicit ec: ExecutionContext): Future[Boolean] = {
    val p = Promise[Boolean]()

    client1.getBlock(hash).onComplete {
      case Success(_) => p.success(true)
      case Failure(_) => p.success(false)
    }

    p.future
  }

  def startedBitcoindRpcClient(
      instance: BitcoindInstance = BitcoindRpcTestUtil.instance())(
      implicit system: ActorSystem): Future[BitcoindRpcClient] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    //start the bitcoind instance so eclair can properly use it
    val rpc = new BitcoindRpcClient(instance)
    val startedF = rpc.start()

    val blocksToGenerate = 102
    //fund the wallet by generating 102 blocks, need this to get over coinbase maturity
    val generatedF = startedF.flatMap { _ =>
      rpc.generate(blocksToGenerate)
    }

    def areBlocksGenerated(): Future[Boolean] = {
      rpc.getBlockCount.map { count =>
        count >= blocksToGenerate
      }
    }

    val blocksGeneratedF = generatedF.flatMap { _ =>
      AsyncUtil.retryUntilSatisfiedF(
        () => areBlocksGenerated(),
        duration = 1.seconds
      )
    }

    val result = blocksGeneratedF.map(_ => rpc)

    result
  }
}

object BitcoindRpcTestUtil extends BitcoindRpcTestUtil
