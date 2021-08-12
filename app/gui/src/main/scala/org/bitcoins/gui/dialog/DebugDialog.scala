package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.scene.control.{Button, ButtonType, Dialog}
import scalafx.scene.layout.VBox
import scalafx.stage.{Modality, Window}

import java.awt.Desktop
import java.io.File
import java.nio.file.{Files, Paths}

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
          // Open file in default log reader per OS
          Desktop.getDesktop().edit(file)
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
