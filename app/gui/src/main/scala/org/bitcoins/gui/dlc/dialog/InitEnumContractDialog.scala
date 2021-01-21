package org.bitcoins.gui.dlc.dialog

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.EnumContractDescriptor
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil.setNumericInput
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import scala.collection._

object InitEnumContractDialog {

  def showAndWait(parentWindow: Window): Option[
    (EnumContractDescriptor, Option[OracleAnnouncementTLV])] = {
    val dialog =
      new Dialog[
        Option[(EnumContractDescriptor, Option[OracleAnnouncementTLV])]]() {
        initOwner(parentWindow)
        title = "Initialize Demo Oracle"
        headerText = "Enter contract outcomes and their outcome values"
      }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    val fields: mutable.Map[Int, (TextField, TextField)] = mutable.Map.empty

    val announcementTF = new TextField() {
      promptText = "(optional)"
    }

    var nextRow: Int = 2
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(new Label("Announcement"), 0, 0)
      add(announcementTF, 1, 0)
      add(new Label("Outcomes"), 0, 1)
      add(new Label("Values"), 1, 1)
    }

    def addOutcomeRow(): Unit = {

      val outcomeTF = new TextField()
      val amtTF = new TextField() {
        promptText = "Satoshis"
      }
      setNumericInput(amtTF)

      val row = nextRow
      val _ = fields.put(row, (outcomeTF, amtTF))

      gridPane.add(outcomeTF, 0, row)
      gridPane.add(amtTF, 1, row)

      nextRow += 1
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    addOutcomeRow()
    addOutcomeRow()

    val addPointButton: Button = new Button("+ Outcome") {
      onAction = _ => addOutcomeRow()
    }

    val bottom = new HBox() {
      spacing = 10
      alignment = Pos.BottomRight
      children = Vector(addPointButton)
    }

    dialog.dialogPane().content = new VBox(gridPane, bottom)

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val inputs = fields.values.flatMap {
          case (str, value) =>
            if (str.text.value.nonEmpty && value.text.value.nonEmpty)
              Some((str.text(), value.text()))
            else None
        }
        val contractMap = inputs.map {
          case (str, value) =>
            EnumOutcome(str) -> Satoshis(BigInt(value))
        }.toVector

        val announcementStr = announcementTF.text.value
        val announcementOpt = if (announcementStr.nonEmpty) {
          Some(OracleAnnouncementTLV(announcementStr))
        } else None

        Some((EnumContractDescriptor(contractMap), announcementOpt))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(
            Some(
              (contractInfo: EnumContractDescriptor,
               announcementOpt: Option[_]))) =>
        Some(
          (contractInfo,
           announcementOpt.asInstanceOf[Option[OracleAnnouncementTLV]]))
      case Some(_) | None => None
    }
  }
}
