package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  MultiNonceContractInfo,
  SingleNonceContractInfo
}
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.{DLCPaneModel, DLCPlotUtil, GlobalDLCData}
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

object ViewDLCDialog {

  def showAndWait(
      parentWindow: Window,
      status: DLCStatus,
      model: DLCPaneModel): Unit = {
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
      val contractId: String = DLCStatus
        .getContractId(status)
        .map(_.toHex)
        .getOrElse("")

      add(new TextField() {
            text = contractId
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
      add(new Label("Contract Info:"), 0, row)
      add(new TextField() {
            text = status.contractInfo.hex
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
      add(new TextField() {
            text = DLCStatus.getFundingTxId(status).map(_.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Closing TxId:"), 0, row)
      add(new TextField() {
            text = DLCStatus.getClosingTxId(status).map(_.hex).getOrElse("")
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Oracle Signatures:"), 0, row)

      val sigsOpt: Option[String] = DLCStatus
        .getOracleSignatures(status)
        .map(_.map(_.hex).mkString(","))

      val node: Node = sigsOpt match {
        case Some(sigs) =>
          new TextField() {
            text = sigs
            editable = false
          }
        case None =>
          new Button("Execute") {
            onAction = _ => {
              // Set data for this DLC
              GlobalDLCData.lastContractId = contractId
              GlobalDLCData.lastOracleSig = ""
              model.onExecute()
            }
          }
      }
      add(node, columnIndex = 1, rowIndex = row)

      row += 1
      status.contractInfo match {
        case _: SingleNonceContractInfo => ()
        case MultiNonceContractInfo(outcomeValueFunc,
                                    base,
                                    numDigits,
                                    totalCollateral) =>
          val previewGraphButton: Button = new Button("Preview Graph") {
            onAction = _ => {
              DLCPlotUtil.plotCETsWithOriginalCurve(
                base,
                numDigits,
                outcomeValueFunc,
                totalCollateral,
                RoundingIntervals.noRounding)
              ()
            }
          }

          add(previewGraphButton, 1, row)
      }
    }

    val _ = dialog.showAndWait()
  }
}
