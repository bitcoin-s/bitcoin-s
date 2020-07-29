package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.scene.control.{ButtonType, Dialog, TextArea}
import scalafx.stage.Window

object AboutDialog {

  def showAndWait(parentWindow: Window): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      title = "About"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.Close)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    val version: String = getClass.getPackage.getImplementationVersion

    dialog.dialogPane().content = new TextArea {
      text =
        s"Bitcoin-S v$version\n\nRepo: https://github.com/bitcoin-s/bitcoin-s"
      editable = false
    }

    val _ = dialog.showAndWait()
  }
}
