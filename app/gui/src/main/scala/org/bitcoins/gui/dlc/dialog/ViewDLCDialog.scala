package org.bitcoins.gui.dlc.dialog

import org.bitcoins.core.protocol.dlc.DLCStatus.Offered
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.tlv.{
  EnumOutcome,
  SignedNumericOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.{DLCPaneModel, DLCPlotUtil, GlobalDLCData}
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.stage.Window

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Instant, ZoneOffset}

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
      add(new Label("Event Id:"), 0, row)
      add(
        new TextField() {
          text =
            status.oracleInfo.singleOracleInfos.head.announcement.eventTLV.eventId
          editable = false
        },
        columnIndex = 1,
        rowIndex = row
      )

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
      add(new Label("Fee Rate:"), 0, row)
      add(new TextField() {
            text = s"${status.feeRate.toLong} sats/vbyte"
            editable = false
          },
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(new Label("Contract Timeout:"), 0, row)
      add(
        new TextField() {
          text = {
            val epoch = status.timeouts.contractTimeout.toUInt32.toLong
            val instant = Instant.ofEpochSecond(epoch).atOffset(ZoneOffset.UTC)
            DateTimeFormatter
              .ofLocalizedDate(FormatStyle.MEDIUM)
              .format(instant)
          }
          editable = false
        },
        columnIndex = 1,
        rowIndex = row
      )

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
      status.contractInfo.contractDescriptor match {
        case _: EnumContractDescriptor => ()
        case descriptor: NumericContractDescriptor =>
          val previewGraphButton: Button = new Button("Preview Graph") {
            onAction = _ => {
              val payoutCurve = if (status.isInitiator) {
                descriptor.outcomeValueFunc
              } else {
                descriptor
                  .flip(status.totalCollateral.satoshis)
                  .outcomeValueFunc
              }

              val outcomeOpt = status match {
                case claimed: ClaimedDLCStatus =>
                  claimed.oracleOutcome.outcome match {
                    case EnumOutcome(_) | SignedNumericOutcome(_, _) => None
                    case UnsignedNumericOutcome(digits) =>
                      Some(digits)
                  }
                case _: Offered | _: AcceptedDLCStatus =>
                  None
              }

              DLCPlotUtil.plotCETs(base = 2,
                                   descriptor.numDigits,
                                   payoutCurve,
                                   status.contractInfo.totalCollateral,
                                   descriptor.roundingIntervals,
                                   outcomeOpt)
              ()
            }
          }

          add(previewGraphButton, 1, row)
      }
    }

    val _ = dialog.showAndWait()
  }
}
