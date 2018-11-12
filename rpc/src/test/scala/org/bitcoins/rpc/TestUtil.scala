package org.bitcoins.rpc

import java.io.{ File, PrintWriter }
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.{ NetworkParameters, RegTest }
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.rpc.config.{ BitcoindAuthCredentials, BitcoindInstance }

import scala.async.Async.{ async, await }
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

trait TestUtil extends BitcoinSLogger {

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
    if (pruneMode) {
      pw.write("prune=1\n")
    }
    pw.close()
    BitcoindAuthCredentials(username, pass, f)
  }

  def instance(
    port: Int = randomPort,
    rpcPort: Int = randomPort,
    pruneMode: Boolean = false): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    BitcoindInstance(
      network,
      uri,
      rpcUri,
      authCredentials(uri, rpcUri, pruneMode))
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
    servers.foreach(server => {
      server.start()
      RpcUtil.awaitServer(server)
    })
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
    val connectedF = from.getAddedNodeInfo(to.getDaemon.uri)
      .map(info => info.nonEmpty && info.head.connected.contains(true))
    async {
      val connected = await(connectedF)
      RpcUtil.awaitCondition(
        connected,
        duration,
        maxTries)
    }
  }

  def awaitSynced(
    client1: BitcoindRpcClient,
    client2: BitcoindRpcClient,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher
    val blockCount1F = client1.getBlockCount
    val blockCount2F = client2.getBlockCount
    async {
      val blockCount1 = await(blockCount1F)
      val blockCount2 = await(blockCount2F)
      RpcUtil.awaitCondition(
        blockCount1 == blockCount2,
        duration, maxTries)
    }
  }

  def awaitDisconnected(
    from: BitcoindRpcClient,
    to: BitcoindRpcClient,
    duration: FiniteDuration = 100.milliseconds,
    maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher
    val disconnectedF = from
      .getAddedNodeInfo(to.getDaemon.uri)
      .map(info => info.isEmpty || !info.head.connected.contains(true))
    async {
      val disconnected = await(disconnectedF)
      RpcUtil.awaitCondition(
        disconnected,
        duration,
        maxTries)
    }
  }

  /** Returns a pair of RpcClients that are connected with 100 blocks in the chain */
  def createNodePair(
    port1: Int = randomPort,
    rpcPort1: Int = randomPort,
    port2: Int = randomPort,
    rpcPort2: Int = randomPort)(implicit system: ActorSystem): Future[(BitcoindRpcClient, BitcoindRpcClient)] = {
    implicit val m: ActorMaterializer = ActorMaterializer.create(system)
    implicit val ec: ExecutionContext = m.executionContext
    async {
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

      await(client1.addNode(client2.getDaemon.uri, "add"))
      val try3 = Try(awaitConnection(client1, client2))
      if (try3.isFailure) {
        deleteNodePair(client1, client2)
        throw try3.failed.get
      }

      await(client1.generate(100))
      val try4 = Try(awaitSynced(client1, client2))
      if (try4.isFailure) {
        deleteNodePair(client1, client2)
        throw try4.failed.get
      }
      (client1, client2)
    }
  }

  def fundBlockChainTransaction(
    sender: BitcoindRpcClient,
    address: BitcoinAddress,
    amount: Bitcoins)(implicit ec: ExecutionContext): Future[DoubleSha256Digest] = async {
    val txid = await(fundMemPoolTransaction(sender, address, amount))
    await(sender.generate(1))
    txid
  }

  def fundMemPoolTransaction(
    sender: BitcoindRpcClient,
    address: BitcoinAddress,
    amount: Bitcoins)(implicit ec: ExecutionContext): Future[DoubleSha256Digest] = {
    sender.sendToAddress(address, amount)
  }

  def deleteNodePair(client1: BitcoindRpcClient, client2: BitcoindRpcClient)(implicit actorSystem: ActorSystem): Unit = stopServers(client1, client2)

}

object TestUtil extends TestUtil
