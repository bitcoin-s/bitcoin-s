package org.bitcoins.gui.dlc.dialog

import org.bitcoins.core.protocol.dlc.DLCMessage.SingleNonceContractInfo
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil.setNumericInput
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import scala.collection._

object InitEnumContractDialog {

  def showAndWait(parentWindow: Window): Option[SingleNonceContractInfo] = {
    val dialog =
      new Dialog[Option[SingleNonceContractInfo]]() {
        initOwner(parentWindow)
        title = "Initialize Demo Oracle"
        headerText = "Enter contract outcomes and their outcome values"
      }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    val fields: mutable.Map[Int, (TextField, TextField)] = mutable.Map.empty

    var nextRow: Int = 1
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(new Label("Outcomes"), 0, 0)
      add(new Label("Values"), 1, 0)
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

        Some(SingleNonceContractInfo(contractMap))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(Some(contractInfo: SingleNonceContractInfo)) =>
        Some(contractInfo)
      case Some(_) | None => None
    }
  }
}
