package org.bitcoins.testkit.rpc

import java.net.URI
import java.nio.file.Paths

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPublicKey
}
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
import org.bitcoins.rpc.util.{AsyncUtil, RpcUtil}
import org.bitcoins.util.ListUtil

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util._
import org.bitcoins.rpc.config.BitcoindConfig
import java.io.File

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import java.nio.file.Path
import org.bitcoins.rpc.client.common.BitcoindVersion.Unknown
import org.bitcoins.rpc.client.common.BitcoindVersion.V16
import org.bitcoins.rpc.client.common.BitcoindVersion.V17
import java.nio.file.Files

import org.bitcoins.testkit.util.FileUtil

//noinspection AccessorLikeMethodIsEmptyParen
trait BitcoindRpcTestUtil extends BitcoinSLogger {
  import BitcoindRpcTestUtil.DEFAULT_LONG_DURATION

  type RpcClientAccum =
    mutable.Builder[BitcoindRpcClient, Vector[BitcoindRpcClient]]

  val AKKA_CONFIG: Config = ConfigFactory.load("akka.conf").resolve()

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

  def tmpDir(): File = {
    val f = Paths.get(Properties.tmpDir, randomDirName).toFile
    f.mkdirs()
    f
  }

  /**
    * Standard config used for testing purposes
    */
  def standardConfig: BitcoindConfig = {
    def newUri: URI = new URI(s"http://localhost:${RpcUtil.randomPort}")
    config(uri = newUri,
           rpcUri = newUri,
           zmqPort = RpcUtil.randomPort,
           pruneMode = false)
  }

  def config(
      uri: URI,
      rpcUri: URI,
      zmqPort: Int,
      pruneMode: Boolean): BitcoindConfig = {
    val pass = randomDirName
    val username = "random_user_name"
    val conf = s"""
                  |regtest=1
                  |daemon=1
                  |server=1
                  |
                  |rpcuser=$username
                  |rpcpassword=$pass
                  |rpcport=${rpcUri.getPort}
                  |port=${uri.getPort}
                  |debug=1
                  |walletbroadcast=1
                  |txindex=${if (pruneMode) 0 else 1 /* pruning and txindex are not compatible */}
                  |zmqpubhashtx=tcp://127.0.0.1:$zmqPort
                  |zmqpubhashblock=tcp://127.0.0.1:$zmqPort
                  |zmqpubrawtx=tcp://127.0.0.1:$zmqPort
                  |zmqpubrawblock=tcp://127.0.0.1:$zmqPort
                  |prune=${if (pruneMode) 1 else 0}
    """.stripMargin
    BitcoindConfig(config = conf, datadir = BitcoindRpcTestUtil.tmpDir())
  }

  /**
    * Creates a `bitcoind` config within the system temp
    * directory, writes the file and returns the written
    * file
    */
  def writtenConfig(
      uri: URI,
      rpcUri: URI,
      zmqPort: Int,
      pruneMode: Boolean
  ): Path = {
    val conf = config(uri, rpcUri, zmqPort, pruneMode)

    val datadir = conf.datadir
    val written = BitcoindConfig.writeConfigToFile(conf, datadir)
    logger.debug(s"Wrote conf to ${written}")
    written
  }

  lazy val network: RegTest.type = RegTest

  /** The directory that sbt downloads bitcoind binaries into */
  private[bitcoins] val binaryDirectory = {
    val baseDirectory = {
      val cwd = Paths.get(Properties.userDir)
      if (cwd.endsWith("bitcoind-rpc-test") || cwd.endsWith("eclair-rpc-test")) {
        cwd.getParent()
      } else cwd
    }

    baseDirectory.resolve("binaries").resolve("bitcoind")
  }

  private def getBinary(version: BitcoindVersion): File = version match {
    // default to newest version
    case Unknown => getBinary(BitcoindVersion.newest)
    case known @ (V16 | V17) =>
      import org.bitcoins.core.compat.JavaConverters._
      val versionFolder = Files
        .list(binaryDirectory)
        .iterator()
        .asScala
        .toList
        .filter { f =>
          val isFolder = Files.isDirectory(f)
          val matchesVersion = f.toString.contains {
            // drop leading 'v'
            known.toString.drop(1)
          }
          isFolder && matchesVersion
        }
        // might be multiple versions downloaded for
        // each major version, i.e. 0.16.2 and 0.16.3
        .sorted
        // we want the most recent one
        .last

      versionFolder
        .resolve("bin")
        .resolve("bitcoind")
        .toFile()
  }

  /** Creates a `bitcoind` instance within the user temporary directory */
  def instance(
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort,
      zmqPort: Int = RpcUtil.randomPort,
      pruneMode: Boolean = false,
      versionOpt: Option[BitcoindVersion] = None): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val configFile = writtenConfig(uri, rpcUri, zmqPort, pruneMode)
    val conf = BitcoindConfig(configFile)
    val auth = BitcoindAuthCredentials.fromConfig(conf)
    val binary: File = versionOpt match {
      case Some(version) => getBinary(version)
      case None =>
        Try {
          BitcoindInstance.DEFAULT_BITCOIND_LOCATION
        }.recoverWith {
          case _: RuntimeException =>
            if (Files.exists(
                  BitcoindRpcTestUtil.binaryDirectory
                )) {
              Success(getBinary(BitcoindVersion.newest))
            } else {
              Failure(new RuntimeException(
                "Could not locate bitcoind. Make sure it is installed on your PATH, or if working with Bitcoin-S directly, try running 'sbt downloadBitcoind'"))
            }

        } match {
          case Failure(exception) => throw exception
          case Success(value)     => value
        }
    }
    val instance = BitcoindInstance(network = network,
                                    uri = uri,
                                    rpcUri = rpcUri,
                                    authCredentials = auth,
                                    zmqConfig = ZmqConfig.fromPort(zmqPort),
                                    binary = binary,
                                    datadir = configFile.getParent.toFile())

    instance
  }

  def v16Instance(
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort,
      zmqPort: Int = RpcUtil.randomPort,
      pruneMode: Boolean = false
  ): BitcoindInstance =
    instance(port = port,
             rpcPort = rpcPort,
             zmqPort = zmqPort,
             pruneMode = pruneMode,
             versionOpt = Some(BitcoindVersion.V16))

  def v17Instance(
      port: Int = RpcUtil.randomPort,
      rpcPort: Int = RpcUtil.randomPort,
      zmqPort: Int = RpcUtil.randomPort,
      pruneMode: Boolean = false
  ): BitcoindInstance =
    instance(port = port,
             rpcPort = rpcPort,
             zmqPort = zmqPort,
             pruneMode = pruneMode,
             versionOpt = Some(BitcoindVersion.V17))

  def startServers(servers: Vector[BitcoindRpcClient])(
      implicit ec: ExecutionContext): Future[Unit] = {
    val startedServers = servers.map(_.start())

    Future.sequence(startedServers).map(_ => ())
  }

  /**
    * Stops the given servers and deletes their data directories
    */
  def stopServers(servers: Vector[BitcoindRpcClient])(
      implicit system: ActorSystem): Future[Unit] = {
    implicit val ec: ExecutionContextExecutor = system.getDispatcher

    val serverStops = servers.map { s =>
      val stopF = s.stop()
      FileUtil.deleteTmpDir(s.getDaemon.datadir)
      stopF.onComplete {
        case Failure(exception) =>
          logger.error(s"Could not shut down sever: $exception")
        case Success(_) =>
      }
      for {
        stop <- stopF
        _ <- RpcUtil.awaitConditionF(() => s.isStoppedF)
      } yield stop
    }
    Future.sequence(serverStops).map(_ => ())
  }

  /**
    * Stops the given server and deletes its data directory
    */
  def stopServer(server: BitcoindRpcClient)(
      implicit system: ActorSystem): Future[Unit] = {
    stopServers(Vector(server))
  }

  /**
    * Awaits non-blockingly until the provided clients are connected
    */
  def awaitConnection(
      from: BitcoindRpcClient,
      to: BitcoindRpcClient,
      duration: FiniteDuration = 100.milliseconds,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher

    val isConnected: () => Future[Boolean] = () => {
      from
        .getAddedNodeInfo(to.getDaemon.uri)
        .map { info =>
          info.nonEmpty && info.head.connected.contains(true)
        }
    }

    AsyncUtil.retryUntilSatisfiedF(conditionF = isConnected,
                                   duration = duration,
                                   maxTries = maxTries)
  }

  /**
    * Return index of output of TX `txid` with value `amount`
    *
    * @see function we're mimicking in
    *      [[https://github.com/bitcoin/bitcoin/blob/master/test/functional/test_framework/util.py#L410 Core test suite]]
    */
  def findOutput(
      client: BitcoindRpcClient,
      txid: DoubleSha256DigestBE,
      amount: Bitcoins,
      blockhash: Option[DoubleSha256DigestBE] = None)(
      implicit executionContext: ExecutionContext): Future[UInt32] = {
    client.getRawTransaction(txid, blockhash).map { tx =>
      tx.vout.zipWithIndex
        .find {
          case (output, _) =>
            output.value == amount
        }
        .map { case (_, i) => UInt32(i) }
        .getOrElse(throw new RuntimeException(
          s"Could not find output for $amount in TX ${txid.hex}"))
    }
  }

  /**
    * Generates the specified amount of blocks with all provided clients
    * and waits until they are synced.
    *
    * @return Vector of Blockhashes of generated blocks, with index corresponding to the
    *         list of provided clients
    */
  def generateAllAndSync(clients: Vector[BitcoindRpcClient], blocks: Int = 6)(
      implicit system: ActorSystem): Future[
    Vector[Vector[DoubleSha256DigestBE]]] = {
    import system.dispatcher

    val sliding = ListUtil.rotateHead(clients)

    val initF = Future.successful(Vector.empty[Vector[DoubleSha256DigestBE]])

    val genereratedHashesF = sliding
      .foldLeft(initF) { (accumHashesF, clients) =>
        accumHashesF.flatMap { accumHashes =>
          val hashesF = generateAndSync(clients, blocks)
          hashesF.map(hashes => hashes +: accumHashes)
        }
      }

    genereratedHashesF.map(_.reverse.toVector)
  }

  /**
    * Generates the specified amount of blocks and waits until
    * the provided clients are synced.
    *
    * @return Blockhashes of generated blocks
    */
  def generateAndSync(clients: Vector[BitcoindRpcClient], blocks: Int = 6)(
      implicit system: ActorSystem): Future[Vector[DoubleSha256DigestBE]] = {
    require(clients.length > 1, "Can't sync less than 2 nodes")

    import system.dispatcher

    for {
      hashes <- clients.head.generate(blocks)
      _ <- {
        val pairs = ListUtil.uniquePairs(clients)
        val syncFuts = pairs.map {
          case (first, second) =>
            awaitSynced(first, second)
        }
        Future.sequence(syncFuts)
      }
    } yield hashes
  }

  def awaitSynced(
      client1: BitcoindRpcClient,
      client2: BitcoindRpcClient,
      duration: FiniteDuration = DEFAULT_LONG_DURATION,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    def isSynced(): Future[Boolean] = {
      client1.getBestBlockHash.flatMap { hash1 =>
        client2.getBestBlockHash.map { hash2 =>
          hash1 == hash2
        }
      }
    }

    AsyncUtil.retryUntilSatisfiedF(conditionF = () => isSynced(),
                                   duration = duration,
                                   maxTries = maxTries)
  }

  def awaitSameBlockHeight(
      client1: BitcoindRpcClient,
      client2: BitcoindRpcClient,
      duration: FiniteDuration = DEFAULT_LONG_DURATION,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher

    def isSameBlockHeight(): Future[Boolean] = {
      client1.getBlockCount.flatMap { count1 =>
        client2.getBlockCount.map { count2 =>
          count1 == count2
        }
      }
    }

    AsyncUtil.retryUntilSatisfiedF(conditionF = () => isSameBlockHeight(),
                                   duration = duration,
                                   maxTries = maxTries)
  }

  def awaitDisconnected(
      from: BitcoindRpcClient,
      to: BitcoindRpcClient,
      duration: FiniteDuration = 100.milliseconds,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher

    def isDisconnected(): Future[Boolean] = {
      from
        .getAddedNodeInfo(to.getDaemon.uri)
        .map(info => info.isEmpty || info.head.connected.contains(false))
        .recoverWith {
          case exception: Exception
              if exception.getMessage.contains("Node has not been added") =>
            from.getPeerInfo.map { peerInfo =>
              peerInfo.forall(_.networkInfo.addr != to.instance.uri)
            }
        }

    }

    AsyncUtil.retryUntilSatisfiedF(conditionF = () => isDisconnected(),
                                   duration = duration,
                                   maxTries = maxTries)
  }

  /**
    * Returns a pair of unconnected
    * [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]s
    * with no blocks
    */
  def createUnconnectedNodePair(
      clientAccum: RpcClientAccum = Vector.newBuilder
  )(
      implicit
      system: ActorSystem): Future[(BitcoindRpcClient, BitcoindRpcClient)] = {
    implicit val ec: ExecutionContextExecutor = system.getDispatcher
    val client1: BitcoindRpcClient =
      BitcoindRpcClient.withActorSystem(instance())
    val client2: BitcoindRpcClient =
      BitcoindRpcClient.withActorSystem(instance())

    val start1F = client1.start()
    val start2F = client2.start()

    for {
      _ <- start1F
      _ <- start2F
    } yield {
      clientAccum ++= List(client1, client2)
      (client1, client2)
    }
  }

  def syncPairs(pairs: Vector[(BitcoindRpcClient, BitcoindRpcClient)])(
      implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    val futures = pairs.map {
      case (first, second) => BitcoindRpcTestUtil.awaitSynced(first, second)
    }
    Future.sequence(futures).map(_ => ())
  }

  /**
    * Connects and waits non-blockingly until all the provided pairs of clients
    * are connected
    */
  def connectPairs(pairs: Vector[(BitcoindRpcClient, BitcoindRpcClient)])(
      implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    val addNodesF: Future[Vector[Unit]] = {
      val addedF = pairs.map {
        case (first, second) =>
          first.addNode(second.getDaemon.uri, AddNodeArgument.Add)
      }
      Future.sequence(addedF)
    }

    val connectedPairsF = addNodesF.flatMap { _ =>
      val futures = pairs.map {
        case (first, second) =>
          BitcoindRpcTestUtil
            .awaitConnection(first, second, duration = 10.second)
      }
      Future.sequence(futures)
    }

    connectedPairsF.map(_ => ())
  }

  /**
    * Generates a vector of connected and started RPC clients. They all have
    * spenable money in their wallet.
    */
  private def createNodeSequence[T <: BitcoindRpcClient](
      numNodes: Int,
      version: BitcoindVersion,
      clientAccum: RpcClientAccum)(
      implicit system: ActorSystem): Future[Vector[T]] = {
    import system.dispatcher

    val clients: Vector[T] = (0 until numNodes).map { _ =>
      val rpc = version match {
        case BitcoindVersion.Unknown =>
          BitcoindRpcClient.withActorSystem(BitcoindRpcTestUtil.instance())
        case BitcoindVersion.V16 =>
          BitcoindV16RpcClient.withActorSystem(
            BitcoindRpcTestUtil.v16Instance())
        case BitcoindVersion.V17 =>
          BitcoindV17RpcClient.withActorSystem(
            BitcoindRpcTestUtil.v17Instance())
      }

      // this is safe as long as this method is never
      // exposed as a public method, and that all public
      // methods calling this make sure that the version
      // arg and the type arg matches up
      val rpcT = rpc.asInstanceOf[T]
      clientAccum += rpcT

      rpcT
    }.toVector

    val startF = BitcoindRpcTestUtil.startServers(clients)

    val pairsF = startF.map { _ =>
      ListUtil.uniquePairs(clients)
    }

    for {
      pairs <- pairsF
      _ <- connectPairs(pairs)
      _ <- BitcoindRpcTestUtil.generateAllAndSync(clients, blocks = 200)
    } yield clients
  }

  private def createNodePairInternal[T <: BitcoindRpcClient](
      version: BitcoindVersion,
      clientAccum: RpcClientAccum)(
      implicit system: ActorSystem): Future[(T, T)] = {
    import system.dispatcher

    createNodeSequence[T](numNodes = 2, version, clientAccum).map {
      case first +: second +: _ => (first, second)
      case _: Vector[BitcoindRpcClient] =>
        throw new RuntimeException("Did not get two clients!")
    }
  }

  /**
    * Returns a pair of [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]
    * that are connected with some blocks in the chain
    */
  def createNodePair(clientAccum: RpcClientAccum = Vector.newBuilder)(
      implicit system: ActorSystem): Future[
    (BitcoindRpcClient, BitcoindRpcClient)] =
    createNodePairInternal(BitcoindVersion.Unknown, clientAccum)

  /**
    * Returns a pair of [[org.bitcoins.rpc.client.v16.BitcoindV16RpcClient BitcoindV16RpcClient]]
    * that are connected with some blocks in the chain
    */
  def createNodePairV16(clientAccum: RpcClientAccum = Vector.newBuilder)(
      implicit system: ActorSystem): Future[
    (BitcoindV16RpcClient, BitcoindV16RpcClient)] =
    createNodePairInternal(BitcoindVersion.V16, clientAccum)

  /**
    * Returns a pair of [[org.bitcoins.rpc.client.v17.BitcoindV17RpcClient BitcoindV17RpcClient]]
    * that are connected with some blocks in the chain
    */
  def createNodePairV17(clientAccum: RpcClientAccum = Vector.newBuilder)(
      implicit system: ActorSystem): Future[
    (BitcoindV17RpcClient, BitcoindV17RpcClient)] =
    createNodePairInternal(BitcoindVersion.V17, clientAccum)

  /**
    * Returns a triple of [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]
    * that are connected with some blocks in the chain
    */
  private def createNodeTripleInternal[T <: BitcoindRpcClient](
      version: BitcoindVersion,
      clientAccum: RpcClientAccum
  )(implicit system: ActorSystem): Future[(T, T, T)] = {
    import system.dispatcher

    createNodeSequence[T](numNodes = 3, version, clientAccum).map {
      case first +: second +: third +: _ => (first, second, third)
      case _: Vector[T] =>
        throw new RuntimeException("Did not get three clients!")
    }
  }

  /**
    * Returns a triple of org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient
    * that are connected with some blocks in the chain
    */
  def createNodeTriple(
      clientAccum: RpcClientAccum = Vector.newBuilder
  )(implicit system: ActorSystem): Future[
    (BitcoindRpcClient, BitcoindRpcClient, BitcoindRpcClient)] = {
    createNodeTripleInternal(BitcoindVersion.Unknown, clientAccum)
  }

  /**
    * @return a triple of [[org.bitcoins.rpc.client.v17.BitcoindV17RpcClient BitcoindV17RpcClient]]
    * that are connected with some blocks in the chain
    */
  def createNodeTripleV17(
      clientAccum: RpcClientAccum = Vector.newBuilder
  )(implicit system: ActorSystem): Future[
    (BitcoindV17RpcClient, BitcoindV17RpcClient, BitcoindV17RpcClient)] = {
    createNodeTripleInternal(BitcoindVersion.V17, clientAccum)
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
    * @throws RuntimeException if no versioned
    * [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient]]
    * can be constructed.
    */
  def signRawTransaction(
      signer: BitcoindRpcClient,
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] = Vector.empty
  ): Future[SignRawTransactionResult] =
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

  /**
    * Gets the pubkey (if it exists) asscociated with a given
    * bitcoin address in a version-agnostic manner
    */
  def getPubkey(client: BitcoindRpcClient, address: BitcoinAddress)(
      implicit system: ActorSystem): Future[Option[ECPublicKey]] = {
    import system.dispatcher

    client match {
      case v17: BitcoindV17RpcClient =>
        v17.getAddressInfo(address).map(_.pubkey)
      case v16: BitcoindV16RpcClient =>
        v16.validateAddress(address).map(_.pubkey)
      case other: BitcoindRpcClient =>
        if (other.instance.getVersion == BitcoindVersion.V17) {
          val v17 = new BitcoindV17RpcClient(other.instance)
          v17.getAddressInfo(address).map(_.pubkey)
        } else {
          other.validateAddress(address).map(_.pubkey)
        }
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

  /**
    * @return The first block (after genesis) in the
    *         given node's blockchain
    */
  def getFirstBlock(node: BitcoindRpcClient)(
      implicit
      executionContext: ExecutionContext): Future[
    GetBlockWithTransactionsResult] = {
    node
      .getBlockHash(1)
      .flatMap(node.getBlockWithTransactions)
  }

  /**
    * Produces a confirmed transaction from `sender` to `address`
    * for `amount`
    */
  def fundBlockChainTransaction(
      sender: BitcoindRpcClient,
      address: BitcoinAddress,
      amount: Bitcoins)(
      implicit system: ActorSystem): Future[DoubleSha256DigestBE] = {
    implicit val mat: ActorMaterializer = ActorMaterializer.create(system)
    implicit val ec: ExecutionContextExecutor = mat.executionContext
    fundMemPoolTransaction(sender, address, amount).flatMap { txid =>
      sender.generate(1).map { _ =>
        txid
      }
    }
  }

  /**
    * Produces a unconfirmed transaction from `sender` to `address`
    * for `amount`
    */
  def fundMemPoolTransaction(
      sender: BitcoindRpcClient,
      address: BitcoinAddress,
      amount: Bitcoins)(
      implicit system: ActorSystem): Future[DoubleSha256DigestBE] = {
    import system.dispatcher
    sender
      .createRawTransaction(Vector.empty, Map(address -> amount))
      .flatMap(sender.fundRawTransaction)
      .flatMap { fundedTx =>
        signRawTransaction(sender, fundedTx.hex).flatMap { signedTx =>
          sender.sendRawTransaction(signedTx.hex)
        }
      }
  }

  /**
    * Stops the provided nodes and deletes their data directories
    */
  def deleteNodePair(client1: BitcoindRpcClient, client2: BitcoindRpcClient)(
      implicit executionContext: ExecutionContext): Future[Unit] = {
    val stopsF = List(client1, client2).map { client =>
      client.stop().map { _ =>
        FileUtil.deleteTmpDir(client.getDaemon.datadir)
      }
    }
    Future.sequence(stopsF).map(_ => ())
  }

  /**
    * Checks whether the provided client has seen the given block hash
    */
  def hasSeenBlock(client: BitcoindRpcClient, hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Boolean] = {
    val p = Promise[Boolean]()

    client.getBlock(hash.flip).onComplete {
      case Success(_) => p.success(true)
      case Failure(_) => p.success(false)
    }

    p.future
  }

  def hasSeenBlock(client1: BitcoindRpcClient, hash: DoubleSha256Digest)(
      implicit ec: ExecutionContext): Future[Boolean] = {
    hasSeenBlock(client1, hash.flip)
  }

  /**
    * @param clientAccum If provided, the generated client is added to
    *                    this vectorbuilder.
    */
  def startedBitcoindRpcClient(
      instance: BitcoindInstance = BitcoindRpcTestUtil.instance(),
      clientAccum: RpcClientAccum = Vector.newBuilder)(
      implicit system: ActorSystem): Future[BitcoindRpcClient] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    require(
      instance.datadir.getPath().startsWith(Properties.tmpDir),
      s"${instance.datadir} is not in user temp dir! This could lead to bad things happening.")

    //start the bitcoind instance so eclair can properly use it
    val rpc = BitcoindRpcClient.withActorSystem(instance)
    val startedF = rpc.start()

    val blocksToGenerate = 102
    //fund the wallet by generating 102 blocks, need this to get over coinbase maturity
    val generatedF = startedF.flatMap { _ =>
      clientAccum += rpc
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
        duration = DEFAULT_LONG_DURATION
      )
    }

    val result = blocksGeneratedF.map(_ => rpc)

    result
  }
}

object BitcoindRpcTestUtil extends BitcoindRpcTestUtil {

  /**
    * Used for long running async tasks
    */
  private val DEFAULT_LONG_DURATION = {
    val isCI = Properties.envOrNone("CI").contains("1")
    if (Properties.isMac && isCI) 10.seconds
    else 3.seconds
  }
}
