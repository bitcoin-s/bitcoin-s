package org.bitcoins.gui.sbclient.dialog

import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

object SbClientInvoiceDialog {

  def showAndWait(
      titleStr: String,
      parentWindow: Window): Option[(String, String)] = {
    val dialog = new Dialog[Option[(String, String)]]() {
      initOwner(parentWindow)
      title = titleStr
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    val exchangeTF = new TextField()
    val tradingPairTF = new TextField()

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

      addRow("Exchange", exchangeTF)
      addRow("Trading Pair", tradingPairTF)
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== exchangeTF.text.isEmpty || tradingPairTF.text.isEmpty

    Platform.runLater(exchangeTF.requestFocus())

    // When the OK button is clicked, convert the result to a T.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        Some((exchangeTF.text(), tradingPairTF.text()))
      } else None

    dialog.showAndWait() match {
      case Some(Some((exchange: String, tradingPair: String))) =>
        Some((exchange, tradingPair))
      case Some(_) | None => None
    }
  }
}
