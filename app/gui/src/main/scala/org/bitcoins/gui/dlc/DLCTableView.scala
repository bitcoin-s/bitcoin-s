package org.bitcoins.gui.dlc

import org.bitcoins.core.protocol.dlc.SingleOracleInfo
import org.bitcoins.core.protocol.dlc.DLCStatus._
import org.bitcoins.core.protocol.dlc.{AcceptedDLCStatus, DLCStatus}
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.control.{ContextMenu, MenuItem, TableColumn, TableView}

class DLCTableView(model: DLCPaneModel) {

  val tableView: TableView[DLCStatus] = {
    val paramHashCol = new TableColumn[DLCStatus, String] {
      text = "Temp Contract Id"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Temp Contract Id",
                           status.value.tempContractId.hex)
      }
    }

    val contractIdCol = new TableColumn[DLCStatus, String] {
      text = "Contract Id"
      prefWidth = 150
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
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status, "Status", status.value.statusString)
      }
    }

    val initiatorCol = new TableColumn[DLCStatus, String] {
      text = "Initiator"
      prefWidth = 80
      cellValueFactory = { status =>
        val str = if (status.value.isInitiator) {
          "Yes"
        } else {
          "No"
        }
        new StringProperty(status, "Initiator", str)
      }
    }

    val collateralCol = new TableColumn[DLCStatus, String] {
      text = "Collateral"
      prefWidth = 110
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Collateral",
                           status.value.localCollateral.toString)
      }
    }

    val oracleCol = new TableColumn[DLCStatus, String] {
      text = "Oracle"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(
          status,
          "Oracle",
          status.value.oracleInfo.asInstanceOf[SingleOracleInfo].publicKey.hex
        ) // FIXME
      }
    }

    val eventCol = new TableColumn[DLCStatus, String] {
      text = "Event"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Event",
                           status.value.oracleInfo
                             .asInstanceOf[SingleOracleInfo]
                             .nonces
                             .map(_.hex)
                             .mkString("")
        ) // FIXME
      }
    }

    val contractMaturityCol = new TableColumn[DLCStatus, String] {
      text = "Contract Mat."
      prefWidth = 110
      cellValueFactory = { status =>
        new StringProperty(
          status,
          "Contract Maturity",
          status.value.timeouts.contractMaturity.toUInt32.toLong.toString)
      }
    }

    new TableView[DLCStatus](model.dlcs) {
      columns ++= Seq(paramHashCol,
                      contractIdCol,
                      statusCol,
                      initiatorCol,
                      collateralCol,
                      oracleCol,
                      eventCol,
                      contractMaturityCol)
      margin = Insets(10, 0, 10, 0)

      val infoItem: MenuItem = new MenuItem("View DLC") {
        onAction = _ => {
          val dlc = selectionModel.value.getSelectedItem
          model.viewDLC(dlc)
        }
      }

      contextMenu = new ContextMenu() {
        items ++= Vector(infoItem)
      }
    }
  }

}
