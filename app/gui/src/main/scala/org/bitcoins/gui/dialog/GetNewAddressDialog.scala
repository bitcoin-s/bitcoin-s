package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.scene.control.{ButtonType, Dialog, TextArea}
import scalafx.stage.Window

object GetNewAddressDialog {

  def showAndWait(parentWindow: Window, address: String): Unit = {
    showAndWait(parentWindow, StringProperty(address))
  }

  def showAndWait(parentWindow: Window, address: StringProperty): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      title = "New Address"
    }

    // TODO make a button to copy the address to clipboard

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    dialog.dialogPane().content = new TextArea {
      text <== address
      editable = false
    }

    val _ = dialog.showAndWait()
  }
}
