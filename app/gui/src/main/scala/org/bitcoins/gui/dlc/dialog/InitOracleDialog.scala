package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import scalafx.stage.Window
import scodec.bits.ByteVector

object InitOracleDialog {

  def showAndWait(
      parentWindow: Window,
      numOutcomes: Int): Option[(Vector[String], ContractInfo)] = {
    val dialog = new Dialog[Option[(Vector[String], ContractInfo)]]() {
      initOwner(parentWindow)
      title = "Create Contract Info"
      headerText = "Enter contract outcomes and their outcome values"
    }

    val fields =
      (0 until numOutcomes).map(_ =>
        (new TextField(),
         new TextField() {
           promptText = "Satoshis"
         }))

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    dialog.dialogPane().content = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      add(new Label("Outcomes"), 0, 0)
      add(new Label("Values"), 1, 0)

      var nextRow: Int = 1

      fields.foreach {
        case (str, value) =>
          add(str, 0, nextRow)
          add(value, 1, nextRow)
          nextRow += 1
      }
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== fields
      .map { case (str, value) => str.text.isEmpty || value.text.isEmpty }
      .reduce(_ || _)

    // Request focus on the first field by default.
    Platform.runLater(fields.head._1.requestFocus())

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val inputs = fields.map {
          case (str, value) => (str.text(), value.text())
        }
        val contractMap = inputs.map {
          case (str, value) =>
            val hash = CryptoUtil.sha256(ByteVector(str.getBytes)).flip
            hash -> Satoshis(BigInt(value))
        }.toMap

        val outcomes = inputs.map(_._1).toVector

        Some((outcomes, ContractInfo(contractMap)))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(Some((outcomes: Vector[_], contractInfo: ContractInfo))) =>
        Some((outcomes.map(_.toString), contractInfo))
      case Some(_) | None => None
    }
  }
}
