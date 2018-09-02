package org.bitcoins.eclair.rpc

import java.io.{ File, PrintWriter }
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.config.{ AuthCredentials, DaemonInstance }

trait TestUtil extends BitcoinSLogger {
  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer.create(system)
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
    bitcoinRpcUri: URI): AuthCredentials = {
    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    val conf = new java.io.File(f.getAbsolutePath + "/eclair.conf")
    conf.createNewFile()
    val username = "random_user_name"
    val pass = randomDirName
    val bitcoinUser = "suredbits"
    val bitcoinPass = "abc123"
    val pw = new PrintWriter(conf)
    pw.println("eclair.chain=regtest")
    pw.println("eclair.bitcoind.rpcuser=" + bitcoinUser)
    pw.println("eclair.bitcoind.rpcpassword=\"" + bitcoinPass + "\"")
    pw.println("eclair.bitcoind.rpcport=18332")
    pw.println("eclair.api.enabled=true")
    pw.println("eclair.node.alias = \"" + username + "\"")
    pw.println("eclair.server.port = " + uri.getPort)
    pw.println("eclair.api.password = \"" + pass + "\"")
    pw.println("eclair.api.port = " + rpcUri.getPort)
    pw.close()

    AuthCredentials(username, pass, bitcoinUser, bitcoinPass, f)
  }

  lazy val network = RegTest

  def instance(port: Int = randomPort, rpcPort: Int = randomPort): DaemonInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val bitcoinRpcUri = new URI("http://localhost:" + 18332)
    DaemonInstance(network, uri, rpcUri, bitcoinRpcUri, authCredentials(uri, rpcUri, bitcoinRpcUri))
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
}

object TestUtil extends TestUtil