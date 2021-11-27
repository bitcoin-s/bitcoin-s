package org.bitcoins.gui.dlc

import org.bitcoins.core.dlc.accounting.RateOfReturnUtil
import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil
import scalafx.beans.property.StringProperty
import scalafx.scene.control.TableColumn.SortType
import scalafx.scene.control.TableView.TableViewFocusModel
import scalafx.scene.control._

import java.time.{LocalDateTime, ZoneId}
import java.time.format.{DateTimeFormatter, FormatStyle}

class DLCTableView(model: DLCPaneModel) {

  val tableView: TableView[DLCStatus] = {
    val eventIdCol = new TableColumn[DLCStatus, String] {
      text = "Event Id"
      prefWidth = 230
      cellValueFactory = { status =>
        status.value.contractInfo match {
          case _: SingleContractInfo =>
            val eventIdStr =
              status.value.eventIds.head
            new StringProperty(status, "Event Id", eventIdStr)
          case _: DisjointUnionContractInfo =>
            sys.error(
              s"Disjoint contracts are not supported via the GUI, cannot add to table")
        }
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

    val lastUpdatedCol = new TableColumn[DLCStatus, String] {
      text = "Last Updated"
      prefWidth = 125

      private val dtFormatter: DateTimeFormatter =
        DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT)

      cellValueFactory = { status =>
        val instant = status.value.lastUpdated
        val zone = ZoneId.systemDefault()
        val str = dtFormatter.format(instant.atZone(zone))
        new StringProperty(status, "Last Updated", str)
      }
      sortType = SortType.Descending
      comparator = (a: String, b: String) => {
        val dtA = LocalDateTime.parse(a, dtFormatter)
        val dtB = LocalDateTime.parse(b, dtFormatter)

        dtA.compareTo(dtB)
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
          case _: SignedDLCStatus | _: AcceptedDLCStatus | _: Offered =>
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
          case _: SignedDLCStatus | _: AcceptedDLCStatus | _: Offered =>
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
                      totalCollateralCol,
                      lastUpdatedCol)

      sortOrder.addAll(lastUpdatedCol)

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
                dlc.announcements.head
              val url =
                GUIUtil.getAnnouncementUrl(GlobalData.network, primaryOracle)
              GUIUtil.openUrl(url)
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
