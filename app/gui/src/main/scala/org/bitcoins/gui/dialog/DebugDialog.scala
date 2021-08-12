package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.scene.control.{Button, ButtonType, Dialog}
import scalafx.scene.layout.VBox
import scalafx.stage.{Modality, Window}

import java.awt.Desktop
import java.io.File
import java.nio.file.{Files, Paths}
import scala.util.Properties

object DebugDialog {

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
            }
          }
        }
      }
    }

    dialog.dialogPane().content = new VBox {
      minWidth = 300
      minHeight = 300
      children = Seq(openLogButton)
    }

    val _ = dialog.show()
  }
}
