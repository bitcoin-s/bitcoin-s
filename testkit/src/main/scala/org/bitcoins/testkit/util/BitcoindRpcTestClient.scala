package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil

import java.nio.file.{Files, Path}
import scala.concurrent.Future

/** Helper class to start a bitcoind client with the given binary */
case class BitcoindRpcTestClient(
    override val binary: Path,
    version: BitcoindVersion)(implicit system: ActorSystem)
    extends RpcBinaryUtil[BitcoindRpcClient] {
  require(Files.exists(binary),
          s"Path did not exist! got=${binary.toAbsolutePath.toString}")
  import system.dispatcher

  private lazy val bitcoindInstance: BitcoindInstance = {
    BitcoindRpcTestUtil.getInstance(bitcoindVersion = version,
                                    binaryDirectory = binaryDirectory)
  }

  /** Cached client. This is defined if start() has been called
    * else None
    */
  private var clientOpt: Option[BitcoindRpcClient] = None

  override def start(): Future[BitcoindRpcClient] = {
    clientOpt match {
      case Some(client) => Future.successful(client)
      case None =>
        val clientF =
          BitcoindRpcTestUtil.startedBitcoindRpcClient(bitcoindInstance)
        clientF.map { c =>
          clientOpt = Some(c)
          c
        }
    }
  }

  override def stop(): Future[BitcoindRpcClient] = {
    clientOpt match {
      case Some(cli) => cli.stop()
      case None =>
        Future.failed(
          new RuntimeException(s"BitcoindRpcClient was not defined!"))
    }
  }

}

object BitcoindRpcTestClient extends SbtBinaryFactory {

  override val sbtBinaryDirectory: Path =
    TestkitBinaries.baseBinaryDirectory.resolve("bitcoind")

  def fromSbtDownload(bitcoindVersion: BitcoindVersion)(implicit
      system: ActorSystem): BitcoindRpcTestClient = {
    val binary =
      BitcoindRpcTestUtil.getBinary(bitcoindVersion, sbtBinaryDirectory)
    BitcoindRpcTestClient(binary.toPath, bitcoindVersion)
  }
}
