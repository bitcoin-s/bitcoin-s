package org.bitcoins.rpc

import java.io.PrintWriter
import java.net.URI

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.RpcClient
import org.bitcoins.rpc.config.{AuthCredentials, DaemonInstance}

trait TestUtil extends BitcoinSLogger {

  def randomDirName: String =
    0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /** Creates a datadir and places the username/password combo
    * in the bitcoin.conf in the datadir */
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
    AuthCredentials(username, pass, d)
  }

  lazy val network = RegTest

  def instance(port: Int, rpcPort: Int, pruneMode: Boolean = false): DaemonInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    DaemonInstance(network, uri, rpcUri, authCredentials(uri, rpcUri, pruneMode))
  }

  def startNodes(clients: Vector[RpcClient]): Unit = {
    clients.foreach(_.start())
  }

  def deleteTmpDir(path: String): Boolean = {
    val dir = new java.io.File(path)
    if (!dir.isDirectory) {
      dir.delete()
    } else {
      dir.listFiles().foreach(file => deleteTmpDir(file.getAbsolutePath))
      dir.delete()
    }
  }
}

object TestUtil extends TestUtil
