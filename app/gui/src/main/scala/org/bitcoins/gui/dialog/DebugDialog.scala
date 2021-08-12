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
          val file = new File(path.toString)
          // Ubuntu seems to support Desktop and Desktop.open(), but hangs on opening file
          if (Properties.isLinux) {
            val _ = Runtime
              .getRuntime()
              .exec("/usr/bin/evince \"" + file.getAbsolutePath() + "\"")
          } else if (
            Desktop.isDesktopSupported && Desktop
              .getDesktop()
              .isSupported(Desktop.Action.OPEN)
          ) {
            // Open file in default log reader per OS
            Desktop.getDesktop().open(file)
          } else {
            println(
              "File is missing or Desktop operations are not supported on this platform")
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
