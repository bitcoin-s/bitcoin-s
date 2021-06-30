package org.bitcoins.bundle.gui

import akka.actor.ActorSystem
import com.typesafe.config._
import grizzled.slf4j.Logging
import org.bitcoins.bundle.gui.BundleGUI._
import org.bitcoins.db.AppConfig
import org.bitcoins.gui._
import org.bitcoins.node.NodeType._
import org.bitcoins.node._
import org.bitcoins.server.BitcoinSAppConfig.toNodeConf
import org.bitcoins.server._
import org.bitcoins.server.util.DatadirUtil
import scalafx.beans.property.ObjectProperty
import scalafx.stage.Window

import java.nio.file.{Files, Path}
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class LandingPaneModel()(implicit system: ActorSystem) extends Logging {

  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] = {
    ObjectProperty[Window](null.asInstanceOf[Window])
  }

  def launchWallet(bundleConf: Config, appConfig: BitcoinSAppConfig): Unit = {
    taskRunner.run(
      "Launching Wallet",
      op = {
        import system.dispatcher
        val file = appConfig.baseDatadir.resolve("bitcoin-s-bundle.conf")

        val bundleConfStr = AppConfig.configToString(bundleConf)

        logger.info(s"Writing bundle config to $file")
        Files.write(file, bundleConfStr.getBytes)

        val tmpFile = Files.createTempFile("bitcoin-s-tmp-config", ".conf")

        val finalConfF: Future[Path] = {
          val tmpConf =
            BitcoinSAppConfig.fromConfig(
              bundleConf.withFallback(appConfig.config))
          val netConfF = tmpConf.nodeType match {
            case _: InternalImplementationNodeType =>
              // If we are connecting to a node we cannot
              // know what network it is on now
              Future.successful(ConfigFactory.empty())
            case BitcoindBackend =>
              tmpConf.bitcoindRpcConf.client.getBlockChainInfo.map { info =>
                val networkStr =
                  DatadirUtil.networkStrToDirName(info.chain.name)
                ConfigFactory.parseString(s"bitcoin-s.network = $networkStr")
              }
          }

          netConfF.map { netConf =>
            val finalConf =
              BitcoinSAppConfig.fromDefaultDatadirWithBundleConf(
                Vector(netConf, bundleConf))

            val totalConfStr = AppConfig.configToString(finalConf.config)
            logger.info(s"Writing resolved config to $tmpFile")
            Files.write(tmpFile, totalConfStr.getBytes)
          }
        }

        // Launch wallet
        val promise = Promise[Unit]()
        BitcoinSServer.startedF.map { _ =>
          fetchStartingData()
          changeToWalletGUIScene()
          promise.success(())
        }

        finalConfF.map { path =>
          val extraArgs = Vector("--conf", path.toAbsolutePath.toString)
          val usedArgs = extraArgs ++ args
          // use class base constructor to share the actor system
          new BitcoinSServerMain(usedArgs.toArray, () => system).run()
        }

        Await.result(promise.future, 60.seconds)
      }
    )
  }
}
