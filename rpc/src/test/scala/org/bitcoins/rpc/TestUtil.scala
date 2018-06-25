package org.bitcoins.rpc

import java.io.{File, PrintWriter}
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.RpcClient
import org.bitcoins.rpc.config.{AuthCredentials, DaemonInstance}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Try

trait TestUtil extends BitcoinSLogger {

  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer()
  implicit val ec = m.executionContext

  def randomDirName: String =
    0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /**
    * Creates a datadir and places the username/password combo
    * in the bitcoin.conf in the datadir
    */
  def authCredentials(
      uri: URI,
      rpcUri: URI,
      pruneMode: Boolean): AuthCredentials = {
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
    AuthCredentials(username, pass, f)
  }

  lazy val network = RegTest

  def instance(
      port: Int,
      rpcPort: Int,
      pruneMode: Boolean = false): DaemonInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    DaemonInstance(network,
                   uri,
                   rpcUri,
                   authCredentials(uri, rpcUri, pruneMode))
  }

  def startServers(servers: Vector[RpcClient]): Unit = {
    servers.foreach(_.start())
    servers.foreach(awaitServer(_))
  }

  def deleteTmpDir(dir: File): Boolean = {
    if (!dir.isDirectory) {
      dir.delete()
    } else {
      dir.listFiles().foreach(deleteTmpDir)
      dir.delete()
    }
  }

  def awaitCondition(
      condition: => Boolean,
      duration: Int = 100,
      counter: Int = 0): Unit = {
    logger.debug(s"counter: ${counter.toString}")
    if (counter == 50) {
      throw new RuntimeException("Condition timed out")
    } else if (condition) {
      Unit
    } else {
      Thread.sleep(duration)
      awaitCondition(condition, counter = counter + 1)
    }
  }

  def awaitServer(
      server: RpcClient,
      duration: Int = 100,
      counter: Int = 0): Unit = {
    awaitCondition(server.isStarted, duration, counter)
  }

  def awaitServerShutdown(
      server: RpcClient,
      duration: Int = 300,
      counter: Int = 0): Unit = {
    awaitCondition(!server.isStarted, duration, counter)
  }

  def awaitConnection(
      from: RpcClient,
      to: RpcClient,
      duration: Int = 100,
      counter: Int = 0): Unit = {
    awaitCondition(
      Await.result(
        from
          .getAddedNodeInfo(to.getDaemon.uri)
          .map(info => info.nonEmpty && info.head.connected.contains(true)),
        5.seconds),
      duration,
      counter)
  }

  def awaitSynced(
      client1: RpcClient,
      client2: RpcClient,
      duration: Int = 100,
      counter: Int = 0): Unit = {
    awaitCondition(Await.result(client1.getBlockCount.flatMap { count1 =>
      client2.getBlockCount.map { count2 =>
        count1 == count2
      }
    }, 2.seconds), duration, counter)
  }

  def awaitDisconnected(
      from: RpcClient,
      to: RpcClient,
      duration: Int = 100,
      counter: Int = 0): Unit = {
    awaitCondition(
      Await.result(
        from
          .getAddedNodeInfo(to.getDaemon.uri)
          .map(info => info.isEmpty || !info.head.connected.contains(true)),
        2.seconds),
      duration,
      counter)
  }

  def createNodePair(
      port1: Int,
      rpcPort1: Int,
      port2: Int,
      rpcPort2: Int): Future[(RpcClient, RpcClient)] = {
    val client1: RpcClient = new RpcClient(TestUtil.instance(port1, rpcPort1))
    val client2: RpcClient = new RpcClient(TestUtil.instance(port2, rpcPort2))
    client1.start()
    client2.start()
    val try1 = Try(awaitServer(client1))
    if (try1.isFailure) {
      deleteNodePair(client1, client2)
      throw try1.failed.get
    }
    val try2 = Try(awaitServer(client2))
    if (try2.isFailure) {
      deleteNodePair(client1, client2)
      throw try2.failed.get
    }
    client1.addNode(client2.getDaemon.uri, "add").flatMap { _ =>
      val try3 = Try(awaitConnection(client1, client2))
      if (try3.isFailure) {
        deleteNodePair(client1, client2)
        throw try3.failed.get
      }
      client1.generate(100).map { _ =>
        val try4 = Try(awaitSynced(client1, client2))
        if (try4.isFailure) {
          deleteNodePair(client1, client2)
          throw try4.failed.get
        }
        (client1, client2)
      }
    }
  }

  def deleteNodePair(client1: RpcClient, client2: RpcClient): Unit = {
    client1.stop()
    client2.stop()
    deleteTmpDir(client1.getDaemon.authCredentials.datadir)
    deleteTmpDir(client2.getDaemon.authCredentials.datadir)
  }
}

object TestUtil extends TestUtil
