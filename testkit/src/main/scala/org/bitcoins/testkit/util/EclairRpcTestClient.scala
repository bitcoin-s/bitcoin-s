package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.eclair.rpc.EclairRpcTestUtil

import java.io.File
import java.nio.file.{Files, Path}
import scala.concurrent.Future

/** Helper class to start a eclair client with the given binary */
case class EclairRpcTestClient(
    override val binary: Path,
    bitcoindRpcClientOpt: Option[BitcoindRpcClient])(implicit
    system: ActorSystem)
    extends RpcBinaryUtil[EclairRpcClient] {
  require(Files.exists(binary),
          s"Path did not exist! got=${binary.toAbsolutePath.toString}")
  import system.dispatcher

  private lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    bitcoindRpcClientOpt match {
      case Some(bitcoindRpcClient) => Future.successful(bitcoindRpcClient)
      case None =>
        EclairRpcTestUtil.startedBitcoindRpcClient()
    }
  }

  private lazy val eclairInstanceF: Future[EclairInstance] = {
    bitcoindRpcClientF.map { bitcoind =>
      EclairRpcTestUtil.eclairInstance(bitcoind, None)
    }

  }

  private lazy val eclairRpcClientF: Future[EclairRpcClient] = {
    eclairInstanceF.map(new EclairRpcClient(_, Some(binary.toFile)))
  }

  override def start(): Future[EclairRpcClient] = {
    //should we start bitcoind rpc client here too?
    for {
      rpcClient <- eclairRpcClientF
      started <- rpcClient.start()
    } yield started
  }

  override def stop(): Future[EclairRpcClient] = {
    //should we stop bitcoind rpc client here too?
    for {
      rpcClient <- eclairRpcClientF
      stopped <- rpcClient.stop()
    } yield stopped
  }
}

object EclairRpcTestClient extends SbtBinaryFactory {

  /** Directory where sbt downloads Eclair binaries */
  override val sbtBinaryDirectory: Path =
    TestkitBinaries.baseBinaryDirectory.resolve("eclair")

  def fromSbtDownloadOpt(
      eclairVersionOpt: Option[String],
      eclairCommitOpt: Option[String],
      bitcoindRpcClientOpt: Option[BitcoindRpcClient])(implicit
      system: ActorSystem): Option[EclairRpcTestClient] = {
    val fileOpt =
      getBinary(eclairVersionOpt = eclairVersionOpt,
                eclairCommitOpt = eclairCommitOpt,
                binaryDirectory = sbtBinaryDirectory)

    fileOpt.map(f =>
      EclairRpcTestClient(binary = f.toPath, bitcoindRpcClientOpt))
  }

  def fromSbtDownload(
      eclairVersionOpt: Option[String],
      eclairCommitOpt: Option[String],
      bitcoindRpcClientOpt: Option[BitcoindRpcClient])(implicit
      system: ActorSystem): EclairRpcTestClient = {
    val eclairOpt = fromSbtDownloadOpt(eclairVersionOpt = eclairCommitOpt,
                                       eclairCommitOpt = eclairCommitOpt,
                                       bitcoindRpcClientOpt =
                                         bitcoindRpcClientOpt)
    eclairOpt match {
      case Some(client) => client
      case None =>
        sys.error(
          s"Could not find eclair that was downloaded by sbt " +
            s"with version=$eclairVersionOpt " +
            s"commit=$eclairCommitOpt at " +
            s"path=${sbtBinaryDirectory.toAbsolutePath.toString}")
    }
  }

  /** Path to Jar downloaded by Eclair, if it exists */
  def getBinary(
      eclairVersionOpt: Option[String],
      eclairCommitOpt: Option[String],
      binaryDirectory: Path): Option[File] = {
    val path = binaryDirectory
      .resolve(eclairVersionOpt.getOrElse(EclairRpcClient.version))
      .resolve(
        s"eclair-node-${EclairRpcClient.version}-${eclairCommitOpt.getOrElse(EclairRpcClient.commit)}")
      .resolve("bin")
      .resolve(
        if (sys.props("os.name").toLowerCase.contains("windows"))
          "eclair-node.bat"
        else
          "eclair-node.sh")

    if (Files.exists(path)) {
      Some(path.toFile)
    } else {
      None
    }
  }
}
