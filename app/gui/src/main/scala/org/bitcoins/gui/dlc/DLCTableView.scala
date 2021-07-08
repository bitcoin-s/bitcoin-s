package org.bitcoins.gui.dlc

import org.bitcoins.commons.jsonmodels.ExplorerEnv
import org.bitcoins.core.dlc.accounting.RateOfReturnUtil
import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.gui.util.GUIUtil
import org.bitcoins.gui.{GUI, GlobalData}
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.control.TableColumn.SortType
import scalafx.scene.control.TableView.TableViewFocusModel
import scalafx.scene.control.{ContextMenu, MenuItem, TableColumn, TableView}

import java.awt.Toolkit.getDefaultToolkit
import java.awt.datatransfer.StringSelection

class DLCTableView(model: DLCPaneModel) {

  val tableView: TableView[DLCStatus] = {

    val eventIdCol = new TableColumn[DLCStatus, String] {
      text = "Event Id"
      prefWidth = 160
      cellValueFactory = { status =>
        val eventIdStr =
          status.value.oracleInfo.singleOracleInfos.head.announcement.eventTLV.eventId

        new StringProperty(status, "Event Id", eventIdStr)
      }
    }

    val labelCol = new TableColumn[DLCStatus, String] {
      text = "Label"
      prefWidth = 100
      cellValueFactory = { status =>
        new StringProperty(status, "Label", status.value.label)
      }
    }

    val statusCol = new TableColumn[DLCStatus, String] {
      text = "Status"
      prefWidth = 125
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
      text = "Counter Party Collateral"
      prefWidth = 200
      cellValueFactory = { status =>
        val amt = GUIUtil.numberFormatter.format(
          status.value.remoteCollateral.satoshis.toLong)
        new StringProperty(status, "Counter Party Collateral", s"$amt sats")
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
      prefWidth = 100
      cellValueFactory = { status =>
        status.value match {
          case closed: ClosedDLCStatus =>
            new StringProperty(status, "PNL", s"${closed.pnl}")
          case _: BroadcastedDLCStatus | _: AcceptedDLCStatus | _: Offered =>
            new StringProperty(status, "PNL", "In progress")
        }
      }
    }

    val rorCol = new TableColumn[DLCStatus, String] {
      text = "Rate of Return"
      prefWidth = 125
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
                      labelCol,
                      statusCol,
                      pnlCol,
                      rorCol,
                      collateralCol,
                      otherCollateralCol,
                      totalCollateralCol)
      margin = Insets(10, 0, 10, 0)
      sortOrder.addAll(statusCol)

      val infoItem: MenuItem = new MenuItem("View DLC") {
        onAction = _ => {
          val selected = selectionModel.value
          val dlc = selected.getSelectedItem
          model.viewDLC(dlc)
          focusModel = new TableViewFocusModel(tableView)
        }
      }

      val viewOnExplorer: MenuItem = new MenuItem("View on Oracle Explorer") {
        onAction = _ => {
          val dlc = selectionModel.value.getSelectedItem
          val primaryOracle =
            dlc.oracleInfo.singleOracleInfos.head.announcement
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

            val clipboard = getDefaultToolkit.getSystemClipboard
            val sel = new StringSelection(id.toHex)
            clipboard.setContents(sel, sel)
          }
        }
      }

      val cancelDLCItem: MenuItem = new MenuItem("Cancel DLC") {
        onAction = _ => {
          val dlc = selectionModel.value.getSelectedItem
          model.cancelDLC(dlc)
        }
      }

      contextMenu = new ContextMenu() {
        items ++= Vector(infoItem, viewOnExplorer, copyIdItem, cancelDLCItem)
      }
    }
  }
}
