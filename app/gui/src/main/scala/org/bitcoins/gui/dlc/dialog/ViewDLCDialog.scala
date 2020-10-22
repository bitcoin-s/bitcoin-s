package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.jsonmodels.dlc.DLCStatus._
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
    dialog.resizable = true

    dialog.dialogPane().content = new GridPane() {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      private var row = 0
      add(new Label("Param Hash:"), 0, row)
      add(new TextField() {
            text = dlcStatus.paramHash.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Initiator:"), 0, row)
      add(new TextField() {
            text = if (dlcStatus.isInitiator) "Yes" else "No"
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("State:"), 0, row)
      add(new TextField() {
            text = dlcStatus.statusString
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Temp Contract Id:"), 0, row)
      add(new TextField() {
            text = dlcStatus.tempContractId.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Id:"), 0, row)
      add(new TextField() {
            text = DLCStatus.getContractId(dlcStatus).map(_.toHex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Oracle Info:"), 0, row)
      add(new TextField() {
            text = dlcStatus.offer.oracleInfo.hex
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Fee Rate:"), 0, row)
      add(new TextField() {
            text = s"${dlcStatus.offer.feeRate.toLong} sats/vbyte"
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Maturity:"), 0, row)
      add(new TextField() {
            text =
              dlcStatus.offer.timeouts.contractMaturity.toUInt32.toLong.toString
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Timeout:"), 0, row)
      add(new TextField() {
            text =
              dlcStatus.offer.timeouts.contractTimeout.toUInt32.toLong.toString
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Collateral:"), 0, row)
      add(
        new TextField() {
          val num: Long = if (dlcStatus.isInitiator) {
            dlcStatus.offer.totalCollateral.toLong
          } else {
            dlcStatus match {
              case _: Offered => 0
              case accepted: AcceptedDLCStatus =>
                accepted.accept.totalCollateral.toLong
            }
          }

          text = num.toString
          editable = false
        },
        columnIndex = 1,
        rowIndex = row
      )

      row += 1
      add(new Label("Funding TxId:"), 0, row)
      add(new TextField() {
            text =
              DLCStatus.getFundingTx(dlcStatus).map(_.txIdBE.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Closing TxId:"), 0, row)
      add(new TextField() {
            text =
              DLCStatus.getClosingTx(dlcStatus).map(_.txIdBE.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Oracle Signature:"), 0, row)
      add(new TextField() {
            text =
              DLCStatus.getOracleSignature(dlcStatus).map(_.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

    }

    val _ = dialog.showAndWait()
  }
}
