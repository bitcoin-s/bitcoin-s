package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

object SendDialog {

  def showAndWait(parentWindow: Window): Option[(String, String)] = {
    val dialog = new Dialog[Option[(String, String)]]() {
      initOwner(parentWindow)
      title = "Send"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    val addressTF = new TextField()
    val amountTF = new TextField() {
      promptText = "(sats)"
    }

    dialog.dialogPane().content = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      var nextRow: Int = 0
      def addRow(label: String, textField: TextField): Unit = {
        add(new Label(label), 0, nextRow)
        add(textField, 1, nextRow)
        nextRow += 1
      }

      addRow("Address", addressTF)
      addRow("Amount", amountTF)
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== addressTF.text.isEmpty || amountTF.text.isEmpty

    Platform.runLater(addressTF.requestFocus())

    // When the OK button is clicked, convert the result to a T.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        Some((addressTF.text(), amountTF.text()))
      } else None

    dialog.showAndWait() match {
      case Some(Some((address: String, amount: String))) =>
        Some((address, amount))
      case Some(_) | None => None
    }
  }
}
