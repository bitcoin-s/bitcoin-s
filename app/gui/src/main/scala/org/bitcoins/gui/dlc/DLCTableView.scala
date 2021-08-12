package org.bitcoins.gui.dlc

import org.bitcoins.commons.jsonmodels.ExplorerEnv
import org.bitcoins.core.dlc.accounting.RateOfReturnUtil
import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.gui.{GUI, GlobalData}
import org.bitcoins.gui.util.GUIUtil
import scalafx.beans.property.StringProperty
import scalafx.scene.control.TableColumn.SortType
import scalafx.scene.control.TableView.TableViewFocusModel
import scalafx.scene.control.{
  ContextMenu,
  MenuItem,
  TableColumn,
  TableRow,
  TableView
}

class DLCTableView(model: DLCPaneModel) {

  val tableView: TableView[DLCStatus] = {
    val eventIdCol = new TableColumn[DLCStatus, String] {
      text = "Event Id"
      prefWidth = 230
      cellValueFactory = { status =>
        val eventIdStr =
          status.value.oracleInfo.singleOracleInfos.head.announcement.eventTLV.eventId
        new StringProperty(status, "Event Id", eventIdStr)
      }
    }

    val contractIdCol = new TableColumn[DLCStatus, String] {
      text = "Contract Id"
      prefWidth = 90
      cellValueFactory = { status =>
        val contractIdStr = status.value match {
          case _: Offered => ""
          case signed: AcceptedDLCStatus =>
            signed.contractId.toHex
        }
        new StringProperty(status, "Contract Id", contractIdStr)
      }
    }

    val statusCol = new TableColumn[DLCStatus, String] {
      text = "Status"
      prefWidth = 105
      cellValueFactory = { status =>
        new StringProperty(status, "Status", status.value.statusString)
      }
      sortType = SortType.Ascending
      comparator = (x: String, y: String) =>
        DLCState.fromString(x).order compare DLCState.fromString(y).order
    }

    val collateralCol = new TableColumn[DLCStatus, String] {
      text = "Collateral"
      prefWidth = 110
      cellValueFactory = { status =>
        val amt = GUIUtil.numberFormatter.format(
          status.value.localCollateral.satoshis.toLong)
        new StringProperty(status, "Collateral", s"$amt sats")
      }
    }

    val otherCollateralCol = new TableColumn[DLCStatus, String] {
      text = "Counterparty Collateral"
      prefWidth = 150
      cellValueFactory = { status =>
        val amt = GUIUtil.numberFormatter.format(
          status.value.remoteCollateral.satoshis.toLong)
        new StringProperty(status, "Counterparty Collateral", s"$amt sats")
      }
    }

    val totalCollateralCol = new TableColumn[DLCStatus, String] {
      text = "Total Collateral"
      prefWidth = 125
      cellValueFactory = { status =>
        val amt = GUIUtil.numberFormatter.format(
          status.value.totalCollateral.satoshis.toLong)
        new StringProperty(status, "Total Collateral", s"$amt sats")
      }
    }

    val pnlCol = new TableColumn[DLCStatus, String] {
      text = "Realized PNL"
      prefWidth = 110
      cellValueFactory = { status =>
        status.value match {
          case closed: ClosedDLCStatus =>
            val amt = GUIUtil.numberFormatter.format(closed.pnl.satoshis.toLong)
            new StringProperty(status, "PNL", s"$amt sats")
          case _: BroadcastedDLCStatus | _: AcceptedDLCStatus | _: Offered =>
            new StringProperty(status, "PNL", "In progress")
        }
      }
    }

    val rorCol = new TableColumn[DLCStatus, String] {
      text = "Rate of Return"
      prefWidth = 105
      cellValueFactory = { status =>
        status.value match {
          case closed: ClosedDLCStatus =>
            new StringProperty(
              status,
              "Rate of Return",
              s"${RateOfReturnUtil.prettyPrint(closed.rateOfReturn)}")
          case _: BroadcastedDLCStatus | _: AcceptedDLCStatus | _: Offered =>
            new StringProperty(status, "Rate of Return", "In progress")
        }
      }
    }

    new TableView[DLCStatus](GlobalDLCData.dlcs) {
      columns ++= Seq(eventIdCol,
                      contractIdCol,
                      statusCol,
                      pnlCol,
                      rorCol,
                      collateralCol,
                      otherCollateralCol,
                      totalCollateralCol)
      sortOrder.addAll(statusCol, eventIdCol, contractIdCol)

      rowFactory = { _ =>
        {
          val row = new TableRow[DLCStatus]()

          val infoItem: MenuItem = new MenuItem("View DLC") {
            onAction = _ => {
              val selected = selectionModel.value
              val dlc = selected.getSelectedItem
              model.viewDLC(dlc)
              focusModel = new TableViewFocusModel(tableView)
            }
          }

          val viewOnExplorer: MenuItem = new MenuItem(
            "View on Oracle Explorer") {
            onAction = _ => {
              val dlc = selectionModel.value.getSelectedItem
              val primaryOracle =
                dlc.oracleInfo.singleOracleInfos.head.announcement
              // TODO : GlobalData.network is currently null here, showing error dialog
              val baseUrl =
                ExplorerEnv.fromBitcoinNetwork(GlobalData.network).siteUrl
              val url =
                s"${baseUrl}announcement/${primaryOracle.sha256.hex}"
              GUI.hostServices.showDocument(url)
            }
          }

          val copyIdItem: MenuItem = new MenuItem("Copy Contract Id") {
            onAction = _ => {
              val dlc = selectionModel.value.getSelectedItem
              getContractId(dlc).foreach { id =>
                GlobalDLCData.lastContractId = id.toHex
                GUIUtil.setStringToClipboard(id.toHex)
              }
            }
          }

          val cancelDLCItem: MenuItem = new MenuItem("Cancel DLC") {
            onAction = _ => {
              val dlc = selectionModel.value.getSelectedItem
              model.cancelDLC(dlc)
            }
          }

          val refundDLCItem: MenuItem = new MenuItem("Refund DLC") {
            disable = true
            onAction = _ => {
              val dlc = selectionModel.value.getSelectedItem
              getContractId(dlc).foreach { id =>
                GlobalDLCData.lastContractId = id.toHex
                model.onRefund()
              }
            }
          }

          val executeDLCItem: MenuItem = new MenuItem("Execute DLC") {
            disable = true
            onAction = _ => {
              val dlc = selectionModel.value.getSelectedItem
              getContractId(dlc).foreach { id =>
                GlobalDLCData.lastContractId = id.toHex
                model.onExecute()
              }
            }
          }
          // Enable/disable menu items per selection state
          row.item.onChange { (_, _, newContent) =>
            if (newContent != null) {
              cancelDLCItem.disable = row.item.value.state match {
                case DLCState.Offered | DLCState.Accepted | DLCState.Signed =>
                  false
                case DLCState.Confirmed | DLCState.Broadcasted |
                    DLCState.Claimed | DLCState.Refunded |
                    DLCState.RemoteClaimed =>
                  true
              }
              val disableRefundExecute = row.item.value.state match {
                case DLCState.Broadcasted | DLCState.Confirmed =>
                  false
                case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
                    DLCState.Claimed | DLCState.Refunded |
                    DLCState.RemoteClaimed =>
                  true
              }
              refundDLCItem.disable = disableRefundExecute
              executeDLCItem.disable = disableRefundExecute
            }
          }

          row.contextMenu = new ContextMenu() {
            items ++= Vector(infoItem,
                             viewOnExplorer,
                             copyIdItem,
                             cancelDLCItem,
                             refundDLCItem,
                             executeDLCItem)
          }
          row
        }
      }
    }
  }
}
