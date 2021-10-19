package org.bitcoins.testkit.clightning

import akka.actor.ActorSystem
import com.bitcoins.clightning.rpc.CLightningRpcClient
import com.bitcoins.clightning.rpc.config.CLightningInstanceLocal
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.util.{
  RpcBinaryUtil,
  SbtBinaryFactory,
  TestkitBinaries
}

import java.io.File
import java.nio.file.{Files, Path}
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** Helper class to start a clightning client with the given binary */
case class CLightningRpcTestClient(
    override val binary: Path,
    bitcoindOpt: Option[BitcoindRpcClient])(implicit system: ActorSystem)
    extends RpcBinaryUtil[CLightningRpcClient] {
  require(Files.exists(binary),
          s"Path did not exist! got=${binary.toAbsolutePath.toString}")
  import system.dispatcher

  /** Cached client. This is defined if start() has been called
    * else None
    */
  private var clientOpt: Option[CLightningRpcClient] = None

  private lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    bitcoindOpt match {
      case Some(bitcoindRpcClient) => Future.successful(bitcoindRpcClient)
      case None                    => CLightningRpcTestUtil.startedBitcoindRpcClient()
    }
  }

  private lazy val instanceF: Future[CLightningInstanceLocal] = {
    bitcoindRpcClientF.map { bitcoind =>
      CLightningRpcTestUtil.cLightingInstance(bitcoind)
    }
  }

  private lazy val cLightningRpcClientF: Future[CLightningRpcClient] = {
    instanceF.map(new CLightningRpcClient(_, binary.toFile))
  }

  override def start(): Future[CLightningRpcClient] = {
    clientOpt match {
      case Some(client) => Future.successful(client)
      case None =>
        for {
          clightning <- cLightningRpcClientF

          _ <- clightning.start()
          // wait for rpc server to start
          _ <- TestAsyncUtil.awaitCondition(
            () => clightning.instance.rpcFile.exists(),
            interval = 1.second,
            maxTries = 500)
          _ <- TestAsyncUtil.nonBlockingSleep(7.seconds)
        } yield {
          clientOpt = Some(clightning)
          clightning
        }
    }
  }

  override def stop(): Future[CLightningRpcClient] = {
    clientOpt match {
      case Some(cli) => cli.stop()
      case None =>
        Future.failed(
          new RuntimeException(s"CLightningRpcClient was not defined!"))
    }
  }
}

object CLightningRpcTestClient extends SbtBinaryFactory {

  /** Directory where sbt downloads clightning binaries */
  override val sbtBinaryDirectory: Path =
    TestkitBinaries.baseBinaryDirectory.resolve("clightning")

  def fromSbtDownloadOpt(
      bitcoindRpcClientOpt: Option[BitcoindRpcClient],
      clightningVersionOpt: Option[String] = None
  )(implicit system: ActorSystem): Option[CLightningRpcTestClient] = {
    val fileOpt =
      getBinary(clightningVersionOpt = clightningVersionOpt,
                binaryDirectory = sbtBinaryDirectory)

    for {
      file <- fileOpt
    } yield CLightningRpcTestClient(binary = file.toPath, bitcoindRpcClientOpt)
  }

  def fromSbtDownload(
      bitcoindRpcClientOpt: Option[BitcoindRpcClient],
      clightningVersionOpt: Option[String] = None)(implicit
      system: ActorSystem): CLightningRpcTestClient = {
    val clightningOpt = fromSbtDownloadOpt(
      bitcoindRpcClientOpt = bitcoindRpcClientOpt,
      clightningVersionOpt = clightningVersionOpt)
    clightningOpt match {
      case Some(client) => client
      case None =>
        sys.error(
          s"Could not find clightning that was downloaded by sbt " +
            s"with version=$clightningVersionOpt " +
            s"path=${sbtBinaryDirectory.toAbsolutePath.toString}")
    }
  }

  /** Path to executable downloaded for clightning, if it exists */
  def getBinary(
      clightningVersionOpt: Option[String],
      binaryDirectory: Path): Option[File] = {
    val versionStr = clightningVersionOpt.getOrElse(CLightningRpcClient.version)

    val path = binaryDirectory
      .resolve(versionStr)
      .resolve("usr")
      .resolve("bin")
      .resolve("lightningd")

    if (Files.exists(path)) {
      Some(path.toFile)
    } else None
  }
}
