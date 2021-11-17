package org.bitcoins.testkit.lnd

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.lnd.rpc._
import org.bitcoins.lnd.rpc.config.LndInstance
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.util.{
  RpcBinaryUtil,
  SbtBinaryFactory,
  TestkitBinaries
}

import java.io.File
import java.nio.file.{Files, Path}
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Properties

/** Helper class to start a bitcoind client with the given binary */
case class LndRpcTestClient(
    override val binary: Path,
    bitcoindOpt: Option[BitcoindRpcClient])(implicit system: ActorSystem)
    extends RpcBinaryUtil[LndRpcClient] {
  require(Files.exists(binary),
          s"Path did not exist! got=${binary.toAbsolutePath.toString}")
  import system.dispatcher

  /** Cached client. This is defined if start() has been called
    * else None
    */
  private var clientOpt: Option[LndRpcClient] = None

  private lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    bitcoindOpt match {
      case Some(bitcoindRpcClient) => Future.successful(bitcoindRpcClient)
      case None                    => LndRpcTestUtil.startedBitcoindRpcClient()
    }
  }

  private lazy val lndInstanceF: Future[LndInstance] = {
    bitcoindRpcClientF.map { bitcoind =>
      LndRpcTestUtil.lndInstance(bitcoind)
    }
  }

  private lazy val lndRpcClientF: Future[LndRpcClient] = {
    lndInstanceF.map(new LndRpcClient(_, Some(binary.toFile)))
  }

  override def start(): Future[LndRpcClient] = {
    clientOpt match {
      case Some(client) => Future.successful(client)
      case None =>
        for {
          lnd <- lndRpcClientF

          _ <- lnd.start()
          // Sleep to make sure lnd is ready for RPC requests
          _ <- AsyncUtil.nonBlockingSleep(1.second)

          // Wait for it to be ready
          _ <- AsyncUtil.awaitConditionF(() => lnd.isStarted,
                                         interval = 500.milliseconds,
                                         maxTries = 100)
        } yield {
          clientOpt = Some(lnd)
          lnd
        }
    }
  }

  override def stop(): Future[LndRpcClient] = {
    clientOpt match {
      case Some(cli) => cli.stop()
      case None =>
        Future.failed(new RuntimeException(s"LndRpcClient was not defined!"))
    }
  }
}

object LndRpcTestClient extends SbtBinaryFactory {

  /** Directory where sbt downloads Lnd binaries */
  override val sbtBinaryDirectory: Path =
    TestkitBinaries.baseBinaryDirectory.resolve("lnd")

  def fromSbtDownloadOpt(
      bitcoindRpcClientOpt: Option[BitcoindRpcClient],
      lndVersionOpt: Option[String] = None)(implicit
      system: ActorSystem): Option[LndRpcTestClient] = {
    val fileOpt =
      getBinary(lndVersionOpt = lndVersionOpt,
                binaryDirectory = sbtBinaryDirectory)

    fileOpt.map(f => LndRpcTestClient(binary = f.toPath, bitcoindRpcClientOpt))
  }

  def fromSbtDownload(
      bitcoindRpcClientOpt: Option[BitcoindRpcClient],
      lndVersionOpt: Option[String] = None)(implicit
      system: ActorSystem): LndRpcTestClient = {
    val lndOpt = fromSbtDownloadOpt(lndVersionOpt = lndVersionOpt,
                                    bitcoindRpcClientOpt = bitcoindRpcClientOpt)
    lndOpt match {
      case Some(client) => client
      case None =>
        sys.error(
          s"Could not find lnd that was downloaded by sbt " +
            s"with version=$lndVersionOpt " +
            s"path=${sbtBinaryDirectory.toAbsolutePath.toString}")
    }
  }

  /** Path to executable downloaded for lnd, if it exists */
  def getBinary(
      lndVersionOpt: Option[String],
      binaryDirectory: Path): Option[File] = {
    val versionStr = lndVersionOpt.getOrElse(LndRpcClient.version)

    val platform =
      if (Properties.isLinux) "linux-amd64"
      else if (Properties.isMac) "darwin-amd64"
      else if (Properties.isWin) "windows-amd64"
      else sys.error(s"Unsupported OS: ${Properties.osName}")

    val path = binaryDirectory
      .resolve(s"lnd-$platform-$versionStr")
      .resolve("lnd")

    if (Files.exists(path)) {
      Some(path.toFile)
    } else {
      None
    }
  }
}
