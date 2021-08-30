package org.bitcoins.gui.dialog

import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand.LockUnspent
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, ButtonType, Dialog, ProgressIndicator}
import scalafx.scene.layout.VBox
import scalafx.stage.{Modality, Window}

import java.awt.Desktop
import java.io.File
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Properties, Success}

object DebugDialog extends Logging {

  private val LOGFILE_NAME = "bitcoin-s.log"

  def show(parentWindow: Window): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      initModality(Modality.None)
      title = "Debug Operations"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.Close)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    val openLogButton = new Button("Open Bitcoin-S Log") {
      onAction = _ => {
        // Get root active network directory
        val location = System.getProperty("bitcoins.log.location")
        val path = Paths.get(location, LOGFILE_NAME)
        if (Files.exists(path)) {
          // Ubuntu seems to support Desktop and Desktop.open(), but hangs on opening file
          // This is an issue in JavaFX and the common workaround is to call on another thread
          // I was not having any luck with Platform.runLater wrapping call to Desktop.open getting around the bug
          if (Properties.isLinux) {
            // Work around native file-open alternative that works on Ubuntu
            val _ = Runtime
              .getRuntime()
              .exec(s"/usr/bin/xdg-open ${path.toString}")
          } else if (Desktop.isDesktopSupported) {
            val d = Desktop.getDesktop
            if (d.isSupported(Desktop.Action.OPEN)) {
              // Open file in default log reader per OS
              d.open(new File(path.toString))
            } else {
              logger.error("Desktop.Action.OPEN on log file not supported")
            }
          } else {
            logger.error(
              "This platform is non-Linux or does not support Desktop")
          }
        } else {
          logger.error(
            s"Expected log file location does not exist ${path.toString}")
        }
      }
    }

    val unreserveAllUTXOsButton = new Button("Unreserve All UTXOs")

    val content = new VBox {
      minWidth = 300
      minHeight = 300
      spacing = 10
      children = Seq(openLogButton, unreserveAllUTXOsButton)
    }

    val glassPane = new VBox {
      children = new ProgressIndicator {
        progress = ProgressIndicator.IndeterminateProgress
        visible = true
      }
      alignment = Pos.Center
      visible = false
    }

    lazy val taskRunner = new TaskRunner(content, glassPane)

    unreserveAllUTXOsButton.onAction = _ => {
      taskRunner.run(
        "Unreserve All UTXOs", {
          ConsoleCli.exec(LockUnspent(true, Vector.empty),
                          GlobalData.consoleCliConfig) match {
            case Success(_) => ()
            case Failure(err) =>
              throw err
          }
        }
      )
    }

    dialog.dialogPane().content = content

    val _ = dialog.show()
  }
}
