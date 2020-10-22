package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

object ViewDLCDialog {

  def showAndWait(parentWindow: Window, dlcStatus: DLCStatus): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      title = "View DLC"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.Close)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    dialog.dialogPane().content = new GridPane() {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      add(new Label("Param Hash:"), 0, 0)
      add(new TextField() {
            text = dlcStatus.paramHash.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = 0)

      add(new Label("Initiator:"), 0, 1)
      add(new TextField() {
            text = if (dlcStatus.isInitiator) "Yes" else "No"
            editable = false
          },
          columnIndex = 1,
          rowIndex = 1)

      add(new Label("State:"), 0, 2)
      add(new TextField() {
            text = dlcStatus.statusString
            editable = false
          },
          columnIndex = 1,
          rowIndex = 2)

      add(new Label("Temp Contract Id:"), 0, 3)
      add(new TextField() {
            text = dlcStatus.tempContractId.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = 3)

      add(new Label("Contract Id:"), 0, 4)
      add(new TextField() {
            text = DLCStatus.getContractId(dlcStatus).map(_.toHex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = 4)

      add(new Label("Fee Rate:"), 0, 5)
      add(new TextField() {
            text = s"${dlcStatus.offer.feeRate.toLong} sats/vbyte"
            editable = false
          },
          columnIndex = 1,
          rowIndex = 5)

      add(new Label("Contract Maturity:"), 0, 6)
      add(new TextField() {
            text =
              dlcStatus.offer.timeouts.contractMaturity.toUInt32.toLong.toString
            editable = false
          },
          columnIndex = 1,
          rowIndex = 6)

      add(new Label("Contract Timeout:"), 0, 7)
      add(new TextField() {
            text =
              dlcStatus.offer.timeouts.contractTimeout.toUInt32.toLong.toString
            editable = false
          },
          columnIndex = 1,
          rowIndex = 7)

      add(new Label("Collateral:"), 0, 8)
      add(
        new TextField() {
          val num: Long = if (dlcStatus.isInitiator) {
            dlcStatus.offer.totalCollateral.toLong
          } else {
            dlcStatus
              .asInstanceOf[AcceptedDLCStatus]
              .accept
              .totalCollateral
              .toLong
          }

          text = num.toString
          editable = false
        },
        columnIndex = 1,
        rowIndex = 8
      )

      add(new Label("Funding TxId:"), 0, 9)
      add(new TextField() {
            text =
              DLCStatus.getFundingTx(dlcStatus).map(_.txIdBE.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = 9)

      add(new Label("Closing TxId:"), 0, 10)
      add(new TextField() {
            text =
              DLCStatus.getClosingTx(dlcStatus).map(_.txIdBE.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = 10)

      add(new Label("Oracle Signature:"), 0, 11)
      add(new TextField() {
            text =
              DLCStatus.getOracleSignature(dlcStatus).map(_.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = 11)

    }

    val _ = dialog.showAndWait()
  }
}
