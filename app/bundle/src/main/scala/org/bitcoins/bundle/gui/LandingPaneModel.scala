package org.bitcoins.bundle.gui

import akka.actor.ActorSystem
import com.typesafe.config._
import grizzled.slf4j.Logging
import org.bitcoins.bundle.gui.BundleGUI._
import org.bitcoins.gui._
import org.bitcoins.server.BitcoinSAppConfig.toNodeConf
import org.bitcoins.server._
import scalafx.beans.property.ObjectProperty
import scalafx.stage.Window

import java.nio.file.Files
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.jdk.CollectionConverters._

class LandingPaneModel()(implicit system: ActorSystem) extends Logging {

  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] = {
    ObjectProperty[Window](null.asInstanceOf[Window])
  }

  def launchWallet(config: Config, appConfig: BitcoinSAppConfig): Unit = {
    taskRunner.run(
      "Launching Wallet",
      op = {
        val file = appConfig.baseDatadir.resolve("bitcoin-s-bundle.conf")

        // if the user made changes in the gui, write to file
        // We compare sets so we account for resolving the config
        val confStr = config
          .entrySet()
          .asScala
          .toVector
          .sortBy(_.getKey)
          .map { entry => s"${entry.getKey} = ${entry.getValue.render()}" }
          .mkString("\n")

        Files.write(file, confStr.getBytes)

        // Launch wallet
        val promise = Promise[Unit]()
        BitcoinSServer.startedF.map { _ =>
          fetchStartingData()
          changeToWalletGUIScene()
          promise.success(())
        }(global)

        //use class base constructor to share the actor system
        new BitcoinSServerMain(args.toArray).run()

        Await.result(promise.future, 60.seconds)
      }
    )
  }
}
