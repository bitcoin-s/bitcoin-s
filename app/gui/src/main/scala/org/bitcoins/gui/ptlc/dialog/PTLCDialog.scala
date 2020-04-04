package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand
import org.bitcoins.gui.ptlc.GlobalPTLCData
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.{
  ButtonType,
  Dialog,
  Label,
  TextArea,
  TextInputControl
}
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

abstract class PTLCDialog[T <: CliCommand](
    dialogTitle: String,
    header: String,
    fields: Vector[(String, TextInputControl)], // Vector instead of Map to keep order
    optionalFields: Vector[String] = Vector.empty) {
  private def readCachedValue(key: String, value: String): Unit = {
    fields
      .find(_._1 == key)
      .foreach(_._2.text = value)
  }

  readCachedValue(PTLCDialog.invoiceIdStr, GlobalPTLCData.lastInvoiceId)

  private def writeCachedValue(
      key: String,
      inputs: Vector[(String, String)],
      setter: String => Unit): Unit = {
    inputs
      .find(_._1 == key)
      .foreach(pair => setter(pair._2))
  }

  def constructFromInput(inputs: Map[String, String]): T

  def showAndWait(parentWindow: Window): Option[T] = {
    val dialog = new Dialog[Option[T]]() {
      initOwner(parentWindow)
      title = dialogTitle
      headerText = header
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)

    dialog.dialogPane().content = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      var nextRow: Int = 0
      def addRow(label: String, textField: TextInputControl): Unit = {
        add(new Label(label), 0, nextRow)
        add(textField, 1, nextRow)
        nextRow += 1
      }

      fields.foreach {
        case (fieldStr, filedInput) => addRow(fieldStr, filedInput)
      }
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    val requiredFields =
      fields.filterNot(field => optionalFields.contains(field._1))
    // Simple validation that sufficient data was entered
    okButton.disable <== requiredFields
      .map(_._2.text.isEmpty)
      .reduce(_ || _)

    // Request focus on the first field by default.
    Platform.runLater(fields.head._2.requestFocus())

    // When the OK button is clicked, convert the result to a T.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val inputs = fields.map { case (key, input) => (key, input.text()) }

        writeCachedValue(PTLCDialog.invoiceIdStr,
                         inputs,
                         GlobalPTLCData.lastInvoiceId = _)

        Some(constructFromInput(inputs.toMap))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(someT: Some[T]) => someT
      case Some(_) | None       => None
    }
  }
}

object PTLCDialog {

  def textArea(): TextArea = {
    new TextArea {
      wrapText = true
    }
  }

  val amountStr = "Amount"
  val timeoutStr = "Timeout"
  val invoiceIdStr = "Invoice ID"
  val invoiceStr = "Invoice"
  val acceptStr = "PTLCAccept Message"
  val refundSigStr = "Refund Signature Message"
  val spendTxStr = "PTLC Claim Transaction"
}
