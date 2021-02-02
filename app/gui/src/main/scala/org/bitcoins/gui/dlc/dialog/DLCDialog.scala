package org.bitcoins.gui.dlc.dialog

import java.io.File

import org.bitcoins.cli.CliCommand
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.GlobalDLCData
import scalafx.Includes._
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.{FileChooser, Window}

import scala.util.Properties

abstract class DLCDialog[T <: CliCommand](
    dialogTitle: String,
    header: String,
    fields: Vector[
      (String, Node)
    ], // Vector instead of Map to keep order
    optionalFields: Vector[String] = Vector.empty) {

  private def readCachedValue(key: String, value: String): Unit = {
    fields
      .find(_._1 == key)
      .foreach {
        _._2 match {
          case textInput: TextInputControl =>
            textInput.text = value
          case node: Node =>
            throw new IllegalArgumentException(
              s"Control at $key is not a text input control, got $node")
        }
      }
  }

  readCachedValue(DLCDialog.dlcContractIdStr, GlobalDLCData.lastContractId)
  readCachedValue(DLCDialog.dlcOracleSigStr, GlobalDLCData.lastOracleSig)
  readCachedValue(DLCDialog.oracleAnnouncementStr,
                  GlobalDLCData.lastOracleAnnouncement)
  readCachedValue(DLCDialog.contractInfoStr, GlobalDLCData.lastContractInfo)

  private def writeCachedValue(
      key: String,
      inputs: Vector[(String, String)],
      setter: String => Unit): Unit = {
    inputs
      .find(_._1 == key)
      .foreach(pair => if (pair._2.nonEmpty) setter(pair._2))
  }

  protected def readStringFromNode(node: Node): String = {
    node match {
      case textInputControl: TextInputControl =>
        textInputControl.text.value
      case node: Node =>
        throw new RuntimeException(s"Got unexpected Node, got $node")
    }
  }

  def constructFromInput(inputs: Map[String, Node]): T

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
      def addRow(label: String, node: Node): Unit = {
        add(new Label(label), 0, nextRow)
        add(node, 1, nextRow)
        nextRow += 1
      }

      fields.foreach {
        case (fieldStr, filedInput) => addRow(fieldStr, filedInput)
      }
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    val requiredTextFields =
      fields.filterNot(field => optionalFields.contains(field._1)).collect {
        case (_: String, textInputControl: TextInputControl) => textInputControl
      }
    // Simple validation that sufficient data was entered
    okButton.disable <== {
      val bools = requiredTextFields
        .map(_.text.isEmpty)

      // Need to do this because foldLeft doesn't work nicely
      if (bools.isEmpty) {
        BooleanProperty(false).delegate
      } else bools.reduce(_ || _).delegate
    }

    // When the OK button is clicked, convert the result to a T.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val textInputs = fields.collect {
          case (key: String, input: TextInputControl) => (key, input.text())
        }

        writeCachedValue(DLCDialog.dlcContractIdStr,
                         textInputs,
                         GlobalDLCData.lastContractId = _)
        writeCachedValue(DLCDialog.dlcOracleSigStr,
                         textInputs,
                         GlobalDLCData.lastOracleSig = _)
        writeCachedValue(DLCDialog.oracleAnnouncementStr,
                         textInputs,
                         GlobalDLCData.lastOracleAnnouncement = _)
        writeCachedValue(DLCDialog.contractInfoStr,
                         textInputs,
                         GlobalDLCData.lastContractInfo = _)

        Some(constructFromInput(fields.toMap))
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

  def fileChooserButton[T](handleFile: File => T): Node = {
    new Button("Browse...") {
      onAction = _ => {
        val fileChooser = new FileChooser() {
          initialDirectory = new File(Properties.userHome)
        }

        val selectedFile = fileChooser.showOpenDialog(null)

        if (selectedFile != null) {
          handleFile(selectedFile)
          ()
        }
      }
    }
  }

  var offerDLCFile: Option[File] = None
  var acceptDLCFile: Option[File] = None
  var signDLCFile: Option[File] = None

  val offerFileChosenLabel = new Label("")
  val acceptFileChosenLabel = new Label("")
  val signFileChosenLabel = new Label("")

  val oracleAnnouncementStr = "Oracle Announcement"
  val contractInfoStr = "Contract Info"
  val collateralStr = "Your Collateral"
  val feeRateStr = "Fee Rate"
  val locktimeStr = "Locktime"
  val refundLocktimeStr = "Refund Locktime"

  val fileChosenStr = ""

  val allOfferFields: Map[String, String] = Map[String, String](
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
  val dlcOfferFileStr = "Open Offer from File"

  val dlcAcceptStr = "DLC Accept Message"
  val dlcAcceptFileStr = "Open Accept from File"

  val dlcSigStr = "DLC Signatures"
  val dlcSignFileStr = "Open Sign from File"

  val dlcContractIdStr = "Contract ID"

  val dlcOracleSigStr = "Oracle Signature"

  val dlcMutualCloseOfferStr = "Mutual Close Offer"

  val dlcForceCloseTxStr = "Force Close Transaction"
}
