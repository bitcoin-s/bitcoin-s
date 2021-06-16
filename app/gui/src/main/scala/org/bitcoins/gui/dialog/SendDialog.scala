package org.bitcoins.gui.dialog

import org.bitcoins.cli.CliCommand.{SendCliCommand, SendToAddress, SweepWallet}
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, HBox}
import scalafx.stage.Window

object SendDialog {

  def showAndWait(parentWindow: Window): Option[SendCliCommand] = {
    val dialog = new Dialog[Option[SendCliCommand]]() {
      initOwner(parentWindow)
      title = "Send"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    val addressTF = new TextField()
    val amountTF = new TextField() {
      promptText = "(sats)"
    }

    val maxButton = new Button("Max") {
      onAction = _ => {
        if (amountTF.disable.value) {
          amountTF.text = ""
          amountTF.disable = false
        } else {
          amountTF.text = "MAX"
          amountTF.disable = true
        }
      }
    }

    val feeRateTF = new TextField() {
      text = GlobalData.feeRate.toLong.toString
      promptText = "(optional)"
    }

    dialog.dialogPane().content = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      var nextRow: Int = 0
      def addRow(label: String, node: Node): Unit = {
        add(new Label(label), 0, nextRow)
        add(node, 1, nextRow)
        nextRow += 1
      }

      addRow("Address", addressTF)
      addRow("Amount", new HBox(amountTF, maxButton))
      addRow("Fee Rate (sats/vbyte)", feeRateTF)
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== addressTF.text.isEmpty || amountTF.text.isEmpty

    Platform.runLater(addressTF.requestFocus())

    // When the OK button is clicked, convert the result to a T.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val address = BitcoinAddress(addressTF.text())
        val feeRateStr = feeRateTF.text()
        val feeRateOpt = if (feeRateStr.nonEmpty) {
          Some(SatoshisPerVirtualByte.fromLong(feeRateStr.toLong))
        } else None

        if (amountTF.disable.value) {
          Some(SweepWallet(address, feeRateOpt))
        } else {
          val long = amountTF.text().toLong
          val amount = Bitcoins(Satoshis(long))

          Some(SendToAddress(address, amount, feeRateOpt, noBroadcast = false))
        }
      } else None

    dialog.showAndWait() match {
      case Some(Some(cmd: SendCliCommand)) => Some(cmd)
      case Some(_) | None                  => None
    }
  }
}
