package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

object ViewDLCDialog {

  def showAndWait(parentWindow: Window, status: SerializedDLCStatus): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      title = "View DLC"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.Close)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    dialog.dialogPane().content = new GridPane() {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      private var row = 0
      add(new Label("Param Hash:"), 0, row)
      add(new TextField() {
            text = status.paramHash.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Initiator:"), 0, row)
      add(new TextField() {
            text = if (status.isInitiator) "Yes" else "No"
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("State:"), 0, row)
      add(new TextField() {
            text = status.statusString
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Temp Contract Id:"), 0, row)
      add(new TextField() {
            text = status.tempContractId.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Id:"), 0, row)
      add(
        new TextField() {
          text =
            SerializedDLCStatus.getContractId(status).map(_.toHex).getOrElse("")
          editable = false
        },
        columnIndex = 1,
        rowIndex = row)

      row += 1
      add(new Label("Oracle Info:"), 0, row)
      add(new TextField() {
            text = status.oracleInfo.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Fee Rate:"), 0, row)
      add(new TextField() {
            text = s"${status.feeRate.toLong} sats/vbyte"
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Maturity:"), 0, row)
      add(new TextField() {
            text = status.timeouts.contractMaturity.toUInt32.toLong.toString
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Timeout:"), 0, row)
      add(new TextField() {
            text = status.timeouts.contractTimeout.toUInt32.toLong.toString
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Collateral:"), 0, row)
      add(
        new TextField() {
          text = status.totalCollateral.satoshis.toLong.toString
          editable = false
        },
        columnIndex = 1,
        rowIndex = row
      )

      row += 1
      add(new Label("Funding TxId:"), 0, row)
      add(
        new TextField() {
          text =
            SerializedDLCStatus.getFundingTxId(status).map(_.hex).getOrElse("")
          editable = false
        },
        columnIndex = 1,
        rowIndex = row)

      row += 1
      add(new Label("Closing TxId:"), 0, row)
      add(
        new TextField() {
          text =
            SerializedDLCStatus.getClosingTxId(status).map(_.hex).getOrElse("")
          editable = false
        },
        columnIndex = 1,
        rowIndex = row)

      row += 1
      add(new Label("Oracle Signatures:"), 0, row)
      add(
        new TextField() {
          text = SerializedDLCStatus
            .getOracleSignatures(status)
            .map(_.map(_.hex).mkString(","))
            .getOrElse("")
          editable = false
        },
        columnIndex = 1,
        rowIndex = row
      )

    }

    val _ = dialog.showAndWait()
  }
}
