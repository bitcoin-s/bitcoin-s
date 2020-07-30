package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.GlobalDLCData
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

abstract class DLCDialog[T <: CliCommand](
    dialogTitle: String,
    header: String,
    fields: Vector[
      (String, TextInputControl)
    ], // Vector instead of Map to keep order
    optionalFields: Vector[String] = Vector.empty) {

  private def readCachedValue(key: String, value: String): Unit = {
    fields
      .find(_._1 == key)
      .foreach(_._2.text = value)
  }

  readCachedValue(DLCDialog.dlcEventIdStr, GlobalDLCData.lastEventId)
  readCachedValue(DLCDialog.dlcOracleSigStr, GlobalDLCData.lastOracleSig)
  readCachedValue(DLCDialog.oracleInfoStr, GlobalDLCData.lastOracleInfo)
  readCachedValue(DLCDialog.contractInfoStr, GlobalDLCData.lastContractInfo)

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
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

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

        writeCachedValue(DLCDialog.dlcEventIdStr,
                         inputs,
                         GlobalDLCData.lastEventId = _)
        writeCachedValue(DLCDialog.dlcOracleSigStr,
                         inputs,
                         GlobalDLCData.lastOracleSig = _)
        writeCachedValue(DLCDialog.oracleInfoStr,
                         inputs,
                         GlobalDLCData.lastOracleInfo = _)
        writeCachedValue(DLCDialog.contractInfoStr,
                         inputs,
                         GlobalDLCData.lastContractInfo = _)

        Some(constructFromInput(inputs.toMap))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(someT: Some[T]) => someT
      case Some(_) | None       => None
    }
  }
}

object DLCDialog {

  def textArea(): TextArea = {
    new TextArea {
      wrapText = true
    }
  }

  val oracleInfoStr = "Oracle Info"
  val contractInfoStr = "Contract Info"
  val collateralStr = "Collateral"
  val feeRateStr = "Fee Rate"
  val locktimeStr = "Locktime"
  val refundLocktimeStr = "Refund Locktime"

  val allOfferFields: Map[String, String] = Map[String, String](
    oracleInfoStr -> "",
    contractInfoStr -> "",
    collateralStr -> "Satoshis",
    feeRateStr -> "sats/vbyte (optional)",
    locktimeStr -> "Block or unix time",
    refundLocktimeStr -> "Block or unix time"
  )

  def constructOfferFields(): Vector[(String, TextField)] =
    allOfferFields.map {
      case (label, hint) =>
        (label,
         new TextField() {
           promptText = hint
         })
    }.toVector

  val dlcOfferStr = "DLC Offer"

  val dlcAcceptStr = "DLC Accept Message"

  val dlcSigStr = "DLC Signatures"

  val dlcEventIdStr = "Event ID"

  val dlcOracleSigStr = "Oracle Signature"

  val dlcMutualCloseOfferStr = "Mutual Close Offer"

  val dlcForceCloseTxStr = "Force Close Transaction"
}
