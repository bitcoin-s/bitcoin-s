package org.bitcoins.gui.dlc.dialog

import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.{
  EnumOutcome,
  SignedNumericOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.gui._
import org.bitcoins.gui.dlc.{DLCPaneModel, DLCPlotUtil, GlobalDLCData}
import org.bitcoins.gui.util.GUIUtil
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, HBox, Priority}
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

    dialog.dialogPane().content = buildView(status, model)

    val _ = dialog.showAndWait()
  }

  private def getLabel(label: String): Label = {
    new Label {
      styleClass += "view-dlc-label"
      text = label
    }
  }

  private def getTextField(value: String): TextField = {
    new TextField {
      styleClass += "view-dlc-textfield"
      text = value
      editable = false
    }
  }

  def buildView(status: DLCStatus, model: DLCPaneModel) = {
    status.contractInfo match {
      case singleContractInfo: SingleContractInfo =>
        buildGridPane(status, singleContractInfo, model)
      case _: DisjointUnionContractInfo =>
        sys.error(s"Disjoint union contracts are not supported")
    }
  }

  private def buildGridPane(
      status: DLCStatus,
      singleContractInfo: SingleContractInfo,
      model: DLCPaneModel) = {
    require(status.contractInfo == singleContractInfo,
            s"Conflicting contract infos")
    val closingTxId: StringProperty = StringProperty(
      DLCStatus.getClosingTxId(status).map(_.hex).getOrElse(""))

    new GridPane() {
      alignment = Pos.Center
      padding = Insets(10)
      hgap = 10
      vgap = 5

      private var row = 0
      add(getLabel("DLC Id"), 0, row)
      add(getTextField(status.dlcId.hex), columnIndex = 1, rowIndex = row)

      row += 1
      add(getLabel("Event Id"), 0, row)
      add(getTextField(status.eventIds.head), columnIndex = 1, rowIndex = row)

      row += 1
      add(getLabel("Initiator"), 0, row)
      add(getTextField(if (status.isInitiator) "Yes" else "No"),
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(getLabel("State"), 0, row)
      add(getTextField(status.statusString), columnIndex = 1, rowIndex = row)

      row += 1
      add(getLabel("Contract Id"), 0, row)
      val contractId: String = DLCStatus
        .getContractId(status)
        .map(_.toHex)
        .getOrElse("")

      add(getTextField(contractId), columnIndex = 1, rowIndex = row)

      row += 1
      add(getLabel("Contract Info"), 0, row)

      add(getTextField(status.contractInfo.toTLV.hex),
          columnIndex = 1,
          rowIndex = row)

      status match {
        case closed: ClosedDLCStatus =>
          row += 1
          add(getLabel("My payout"), 0, row)
          add(getTextField(s"${closed.myPayout}"),
              columnIndex = 1,
              rowIndex = row)

          row += 1
          add(getLabel("Counterparty payout"), 0, row)
          add(getTextField(s"${closed.counterPartyPayout}"),
              columnIndex = 1,
              rowIndex = row)

          row += 1
          add(getLabel("PNL"), 0, row)
          add(getTextField(s"${closed.pnl}"), columnIndex = 1, rowIndex = row)

          row += 1
          add(getLabel("Rate of Return"), 0, row)
          add(getTextField(s"${closed.rateOfReturnPrettyPrint}"),
              columnIndex = 1,
              rowIndex = row)
        case _: AcceptedDLCStatus | _: Offered =>
        //do nothing as that stats aren't available
      }

      row += 1
      add(getLabel("Fee Rate"), 0, row)
      add(getTextField(s"${status.feeRate.toLong} sats/vbyte"),
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(getLabel("Contract Timeout"), 0, row)
      add(getTextField(
            GUIUtil.epochToDateString(status.timeouts.contractTimeout)),
          columnIndex = 1,
          rowIndex = row)

      row += 1
      add(getLabel("Collateral"), 0, row)
      add(getTextField(status.totalCollateral.satoshis.toLong.toString),
          columnIndex = 1,
          rowIndex = row)

      row += 1
      // TODO : Status filtering for showing this view vs just the TextField
      val fundingHBox = new HBox() {
        children = Seq(
          new TextField {
            text = DLCStatus.getFundingTxId(status).map(_.hex).getOrElse("")
            editable = false
            hgrow = Priority.Always
          },
          new Button("Rebroadcast") {
            minWidth = 90
            disable = !(status.state == DLCState.Broadcasted)
            onAction = _ => model.rebroadcastFundingTx(status)
          }
        )
      }
      add(getLabel("Funding TxId"), 0, row)
      add(fundingHBox, columnIndex = 1, rowIndex = row)

      row += 1
      // TODO : Status filtering for showing this view vs just the TextField
      val closingHBox = new HBox {
        children = Seq(
          new TextField() {
            text <== closingTxId
            editable = false
            hgrow = Priority.Always
          },
          new Button("Rebroadcast") {
            minWidth = 90
            disable = closingTxId.isEmpty.getValue
            onAction = _ => {
              model.rebroadcastClosingTx(status)
            }
          }
        )
      }
      add(getLabel("Closing TxId"), 0, row)
      add(closingHBox, columnIndex = 1, rowIndex = row)

      row += 1
      add(getLabel("Oracle Signatures"), 0, row)

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
              val res = model.onExecute()

              // Set closing txId in GUI
              closingTxId.value = res
            }
          }
      }
      add(node, columnIndex = 1, rowIndex = row)

      // TODO : Refund button and discriminator

      row += 1
      singleContractInfo.contractDescriptor match {
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
  }
}
