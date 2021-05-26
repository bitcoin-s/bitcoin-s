package org.bitcoins.bundle.gui

import com.typesafe.config._
import org.bitcoins.bundle.gui.BundleGUI._
import org.bitcoins.gui._
import org.bitcoins.server.BitcoinSAppConfig.toNodeConf
import org.bitcoins.server._
import scalafx.beans.property.ObjectProperty
import scalafx.stage.Window

import java.nio.file.Files
import scala.concurrent.ExecutionContext.global
import scala.jdk.CollectionConverters._

class LandingPaneModel() {
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
        BitcoinSServer.startedF.map { _ =>
          fetchStartingData()
          changeToWalletGUIScene()
        }(global)
        BitcoinSServerMain.main(args.toArray)
      }
    )
  }
}
