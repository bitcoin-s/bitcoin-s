package org.bitcoins.rpc

import java.io.{ File, PrintWriter }
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionInput, TransactionOutPoint }
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.config.{ BitcoindAuthCredentials, BitcoindInstance }
import org.bitcoins.rpc.jsonmodels.{ GetBlockWithTransactionsResult, GetTransactionResult }

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.util.{ Failure, Success, Try }

trait BitcoindRpcTestUtil extends BitcoinSLogger {

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

  lazy val network = RegTest

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

  def startServers(servers: Vector[BitcoindRpcClient])(implicit system: ActorSystem): Unit = {
    servers.foreach(_.start())
    servers.foreach(RpcUtil.awaitServer(_))
  }

  /**
   * Stops the given servers, and waits for them (in a blocking manner)
   * until they are stopped. Deletes their temp dir.
   */
  def stopServers(servers: Vector[BitcoindRpcClient])(implicit system: ActorSystem): Unit =
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
    implicit val ec = system.dispatcher

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
    implicit val ec = system.dispatcher

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
    implicit val ec = system.dispatcher

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
    implicit val ec = system.dispatcher

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

  def deleteNodePair(client1: BitcoindRpcClient, client2: BitcoindRpcClient): Unit = {
    client1.stop()
    client2.stop()
    deleteTmpDir(client1.getDaemon.authCredentials.datadir)
    deleteTmpDir(client2.getDaemon.authCredentials.datadir)
  }

  def hasSeenBlock(client1: BitcoindRpcClient, hash: DoubleSha256Digest)(implicit ec: ExecutionContext): Future[Boolean] = {
    val p = Promise[Boolean]()

    client1.getBlock(hash).onComplete {
      case Success(_) => p.success(true)
      case Failure(_) => p.success(false)
    }

    p.future
  }

  /**
   * Returns a pair of unconnected [[BitcoindRpcClient]]s with no blocks
   */
  def createUnconnectedNodePair(
    port1: Int = randomPort,
    rpcPort1: Int = randomPort,
    port2: Int = randomPort,
    rpcPort2: Int = randomPort)(
    implicit
    system: ActorSystem): (BitcoindRpcClient, BitcoindRpcClient) = {

    implicit val m: ActorMaterializer = ActorMaterializer.create(system)
    implicit val ec: ExecutionContext = m.executionContext
    val client1: BitcoindRpcClient = new BitcoindRpcClient(instance(port1, rpcPort1))
    val client2: BitcoindRpcClient = new BitcoindRpcClient(instance(port2, rpcPort2))

    client1.start()
    client2.start()

    val try1 = Try(RpcUtil.awaitServer(client1))
    if (try1.isFailure) {
      deleteNodePair(client1, client2)
      throw try1.failed.get
    }

    val try2 = Try(RpcUtil.awaitServer(client2))
    if (try2.isFailure) {
      deleteNodePair(client1, client2)
      throw try2.failed.get
    }

    (client1, client2)
  }

  /**
   * Returns a pair of [[BitcoindRpcClient]] that are connected with 100 blocks in the chain
   */
  def createNodePair(
    port1: Int = randomPort,
    rpcPort1: Int = randomPort,
    port2: Int = randomPort,
    rpcPort2: Int = randomPort)(implicit system: ActorSystem): Future[(BitcoindRpcClient, BitcoindRpcClient)] = {
    implicit val executionContext: ExecutionContext = system.dispatcher
    val (client1, client2) = createUnconnectedNodePair()
    client1.addNode(client2.getDaemon.uri, AddNodeArgument.Add).map(_ => {
      val try3 = Try(awaitConnection(client1, client2))
      if (try3.isFailure) {
        logger.error(s"Failed to connect client1 and client 2 ${try3.failed.get.getMessage}")
        deleteNodePair(client1, client2)
        throw try3.failed.get
      }
    }).map { _ => client1.generate(100) }
      .map { _ =>
        {
          val try4 = Try(awaitSynced(client1, client2))
          if (try4.isFailure) {
            deleteNodePair(client1, client2)
            throw try4.failed.get
          }
          (client1, client2)
        }
      }
  }

  def createRawCoinbaseTransaction(
    sender: BitcoindRpcClient,
    receiver: BitcoindRpcClient,
    amount: Bitcoins = Bitcoins(1))(implicit executionContext: ExecutionContext): Future[Transaction] = {
    sender.generate(2).flatMap { blocks =>
      sender.getBlock(blocks(0)).flatMap { block0 =>
        sender.getBlock(blocks(1)).flatMap { block1 =>
          sender.getTransaction(block0.tx(0)).flatMap { transaction0 =>
            sender.getTransaction(block1.tx(0)).flatMap { transaction1 =>
              val input0 =
                TransactionOutPoint(
                  transaction0.txid.flip,
                  UInt32(transaction0.blockindex.get))
              val input1 =
                TransactionOutPoint(
                  transaction1.txid.flip,
                  UInt32(transaction1.blockindex.get))
              val sig: ScriptSignature = ScriptSignature.empty
              receiver.getNewAddress.flatMap { address =>
                sender.createRawTransaction(
                  Vector(
                    TransactionInput(input0, sig, UInt32(1)),
                    TransactionInput(input1, sig, UInt32(2))),
                  Map(address -> amount))
              }
            }
          }
        }
      }
    }
  }

  def sendCoinbaseTransaction(
    sender: BitcoindRpcClient,
    receiver: BitcoindRpcClient,
    amount: Bitcoins = Bitcoins(1))(
    implicit
    executionContext: ExecutionContext): Future[GetTransactionResult] = {
    BitcoindRpcTestUtil.
      createRawCoinbaseTransaction(sender, receiver, amount)
      .flatMap {
        transaction =>
          sender.signRawTransaction(transaction).flatMap { signedTransaction =>
            sender
              .generate(100)
              .flatMap { _ => // Can't spend coinbase until depth 100
                sender.sendRawTransaction(
                  signedTransaction.hex,
                  allowHighFees = true).flatMap {
                    transactionHash =>
                      sender.getTransaction(transactionHash)
                  }
              }
          }
      }
  }

  def getFirstBlock(implicit
    node: BitcoindRpcClient,
    executionContext: ExecutionContext): Future[GetBlockWithTransactionsResult] = {
    node.getBlockHash(1).flatMap { hash =>
      node.getBlockWithTransactions(hash)
    }
  }

  def fundBlockChainTransaction(
    sender: BitcoindRpcClient,
    address: BitcoinAddress,
    amount: Bitcoins)(implicit ec: ExecutionContext): Future[DoubleSha256Digest] = {
    fundMemPoolTransaction(sender, address, amount).flatMap { txid =>
      sender.generate(1).map { _ =>
        txid
      }
    }
  }

  def fundMemPoolTransaction(
    sender: BitcoindRpcClient,
    address: BitcoinAddress,
    amount: Bitcoins)(implicit ec: ExecutionContext): Future[DoubleSha256Digest] = {
    sender.sendToAddress(address, amount)
  }

  def startedBitcoindRpcClient(instance: BitcoindInstance = BitcoindRpcTestUtil.instance())(implicit system: ActorSystem): BitcoindRpcClient = {
    implicit val ec = system.dispatcher
    //start the bitcoind instance so eclair can properly use it
    val rpc = new BitcoindRpcClient(instance)(ActorMaterializer.create(system))
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
}

object BitcoindRpcTestUtil extends BitcoindRpcTestUtil