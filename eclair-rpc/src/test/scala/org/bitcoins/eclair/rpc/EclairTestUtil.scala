package org.bitcoins.eclair.rpc

import java.io.{ File, PrintWriter }
import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.config.{ EclairAuthCredentials, EclairInstance }
import org.bitcoins.rpc.RpcUtil
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.bitcoins.rpc.config.{ BitcoindAuthCredentials, BitcoindInstance }

trait EclairTestUtil extends BitcoinSLogger {

  def randomDirName: String =
    0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /**
   * Creates a datadir and places the username/password combo
   * in the bitcoin.conf in the datadir
   */
  def bitcoindAuthCredentials(
    uri: URI,
    rpcUri: URI,
    zmqPort: Int): BitcoindAuthCredentials = {
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

    pw.close()
    BitcoindAuthCredentials(username, pass, f)
  }

  lazy val network = RegTest

  def bitcoindInstance(
    port: Int = randomPort,
    rpcPort: Int = randomPort,
    zmqPort: Int = randomPort): BitcoindInstance = {
    val uri = new URI("http://localhost:" + port)
    val rpcUri = new URI("http://localhost:" + rpcPort)
    val auth = bitcoindAuthCredentials(uri, rpcUri, zmqPort)

    BitcoindInstance(network, uri, rpcUri, auth, Some(zmqPort))
  }

  def startedBitcoindInstance()(implicit system: ActorSystem): BitcoindInstance = {

    val i = bitcoindInstance()

    //start the bitcoind instance so eclair can properly use it
    val rpc = new BitcoindRpcClient(i)(system)
    rpc.start()

    logger.debug(s"Starting bitcoind at ${i.authCredentials.datadir}")
    RpcUtil.awaitServer(rpc)

    //fund the wallet by generating 102 blocks, need this to get over coinbase maturity
    val blockGen = rpc.generate(102)
    i
  }

  /**
   * Creates a datadir and places the username/password combo
   * in the eclair.conf in the datadir
   */
  def eclairAuthCredentials(uri: URI, rpcUri: URI, bitcoind: BitcoindInstance): EclairAuthCredentials = {
    val bitcoindAuth = bitcoind.authCredentials
    val bitcoindRpcUri = bitcoind.rpcUri

    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    val conf = new java.io.File(f.getAbsolutePath + "/eclair.conf")
    conf.createNewFile()
    val username = "random_user_name"
    val pass = randomDirName

    val bitcoindUser = bitcoindAuth.username
    val bitcoindPass = bitcoindAuth.password

    val zmqUri: String = s"tcp://127.0.0.1:${bitcoind.zmqPortOpt.get}"
    val pw = new PrintWriter(conf)
    pw.println("eclair.chain=regtest")

    pw.println(s"eclair.bitcoind.rpcuser=${bitcoindUser}")
    pw.println("eclair.bitcoind.rpcpassword=\"" + bitcoindPass + "\"")
    pw.println(s"eclair.bitcoind.rpcport=${bitcoindRpcUri.getPort}")
    pw.println(s"eclair.bitcoind.host=${bitcoindRpcUri.getHost}")
    pw.println("eclair.bitcoind.zmq =\"" + zmqUri + "\"")

    pw.println("eclair.api.enabled=true")
    pw.println("eclair.node.alias = \"" + username + "\"")
    pw.println("eclair.server.port = " + uri.getPort)
    pw.println("eclair.api.password = \"" + pass + "\"")
    pw.println("eclair.api.port = " + rpcUri.getPort)
    pw.println("eclair.api.binding-ip = \"127.0.0.1\"")
    pw.close()

    EclairAuthCredentials(username, pass, bitcoindUser, bitcoindPass, f)
  }

  lazy val bitcoinNetwork = RegTest

  def eclairInstance(bitcoind: BitcoindInstance, port: Int = randomPort, rpcPort: Int = randomPort)(implicit system: ActorSystem): EclairInstance = {
    val uri = new URI("http://127.0.0.1:" + port)
    val rpcUri = new URI("http://127.0.0.1:" + rpcPort)
    val auth = eclairAuthCredentials(uri, rpcUri, bitcoind)
    EclairInstance(bitcoinNetwork, uri, rpcUri, auth)
  }

  def randomPort: Int = {
    val firstAttempt = Math.abs(scala.util.Random.nextInt % 15000)
    if (firstAttempt < bitcoinNetwork.port) {
      firstAttempt + bitcoinNetwork.port
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

object EclairTestUtil extends EclairTestUtil