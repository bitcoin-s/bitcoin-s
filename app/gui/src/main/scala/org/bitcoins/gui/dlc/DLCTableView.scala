package org.bitcoins.gui.dlc

import org.bitcoins.commons.jsonmodels.dlc.{
  AcceptedSerializedDLCStatus,
  SerializedDLCStatus
}
import org.bitcoins.commons.jsonmodels.dlc.SerializedDLCStatus._
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.control.{ContextMenu, MenuItem, TableColumn, TableView}

class DLCTableView(model: DLCPaneModel) {

  val tableView: TableView[SerializedDLCStatus] = {
    val paramHashCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Temp Contract Id"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Temp Contract Id",
                           status.value.tempContractId.hex)
      }
    }

    val contractIdCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Contract Id"
      prefWidth = 150
      cellValueFactory = { status =>
        val contractIdStr = status.value match {
          case _: SerializedOffered => ""
          case signed: AcceptedSerializedDLCStatus =>
            signed.contractId.toHex
        }

        new StringProperty(status, "Contract Id", contractIdStr)
      }
    }

    val statusCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Status"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status, "Status", status.value.statusString)
      }
    }

    val initiatorCol = new TableColumn[SerializedDLCStatus, String] {
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

    val collateralCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Collateral"
      prefWidth = 110
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Collateral",
                           status.value.localCollateral.toString)
      }
    }

    val oracleCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Oracle"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status, "Oracle", status.value.oracleInfo.pubKey.hex)
      }
    }

    val eventCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Event"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(
          status,
          "Event",
          status.value.oracleInfo.nonces.map(_.hex).mkString(""))
      }
    }

    val contractMaturityCol = new TableColumn[SerializedDLCStatus, String] {
      text = "Contract Mat."
      prefWidth = 110
      cellValueFactory = { status =>
        new StringProperty(
          status,
          "Contract Maturity",
          status.value.timeouts.contractMaturity.toUInt32.toLong.toString)
      }
    }

    new TableView[SerializedDLCStatus](model.dlcs) {
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
