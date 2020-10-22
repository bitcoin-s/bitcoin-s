package org.bitcoins.gui.dlc

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.beans.property._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._

class DLCPane(glassPane: VBox) {

  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  private val resultArea = new TextArea {
    editable = false
    text = "Click on Offer or Accept to begin."
    wrapText = true
  }

  private val demoOracleArea = new TextArea {
    editable = false
    text =
      "Click on Init Demo Oracle to generate example oracle and contract information"
    wrapText = true
  }

  private val numOutcomesTF = new TextField {
    promptText = "Number of Outcomes"
  }

  private val model =
    new DLCPaneModel(resultArea, demoOracleArea, numOutcomesTF)

  model.setUp()

  private val demoOracleButton = new Button {
    text = "Init Demo Oracle"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onInitOracle()
    }
  }

  private val oracleButtonHBox = new HBox {
    children = Seq(numOutcomesTF, demoOracleButton)
    spacing = 15
  }

  private val demoOracleVBox = new VBox {
    children = Seq(demoOracleArea, oracleButtonHBox)
    spacing = 15
  }

  private val offerButton = new Button {
    text = "Offer"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onOffer()
    }
  }

  private val acceptButton = new Button {
    text = "Accept"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onAccept()
    }
  }

  private val signButton = new Button {
    text = "Sign"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onSign()
    }
  }

  private val addSigsButton = new Button {
    text = "Add Sigs"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onAddSigs()
    }
  }

  private val getFundingButton = new Button {
    text = "Get Funding Tx"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onGetFunding()
    }
  }

  private val refundButton = new Button {
    text = "Refund"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onRefund()
    }
  }

  private val executeButton = new Button {
    text = "Execute"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onClose()
    }
  }

  private val initButtonBar = new ButtonBar {
    buttons = Seq(offerButton, signButton)
  }

  private val acceptButtonBar = new ButtonBar {
    buttons = Seq(acceptButton, addSigsButton, getFundingButton)
  }

  private val execButtonBar = new ButtonBar {
    buttons = Seq(refundButton, executeButton)
  }

  private val spaceRegion = new Region()
  private val spaceRegion2 = new Region()

  private val buttonSpacer = new GridPane {
    hgap = 10
    alignment = Pos.Center

    add(initButtonBar, 0, 0)
    add(spaceRegion, 1, 0)
    add(acceptButtonBar, 2, 0)
    add(spaceRegion2, 3, 0)
    add(execButtonBar, 4, 0)
  }

  private val textAreaHBox = new HBox {
    children = Seq(resultArea, demoOracleVBox)
    spacing = 10
  }

  private val tableView: TableView[DLCStatus] = {
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
          case _: DLCStatus.Offered | _: DLCStatus.Accepted =>
            ""
          case signed: SignedDLCStatus =>
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
        val num = if (status.value.isInitiator) {
          status.value.offer.totalCollateral.toLong
        } else {
          status.value
            .asInstanceOf[AcceptedDLCStatus]
            .accept
            .totalCollateral
            .toLong
        }
        new StringProperty(status, "Collateral", num.toString)
      }
    }

    val oracleCol = new TableColumn[DLCStatus, String] {
      text = "Oracle"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Oracle",
                           status.value.offer.oracleInfo.pubKey.hex)
      }
    }

    val eventCol = new TableColumn[DLCStatus, String] {
      text = "Event"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Event",
                           status.value.offer.oracleInfo.rValue.hex)
      }
    }

    val contractMaturityCol = new TableColumn[DLCStatus, String] {
      text = "Contract Mat."
      prefWidth = 110
      cellValueFactory = { status =>
        new StringProperty(
          status,
          "Contract Maturity",
          status.value.offer.timeouts.contractMaturity.toUInt32.toLong.toString)
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

  private val textAreasAndTableViewVBox = new VBox {
    children = Seq(textAreaHBox, tableView)
    spacing = 10
  }

  val borderPane: BorderPane = new BorderPane {
    top = buttonSpacer
    center = textAreasAndTableViewVBox
    bottom = statusLabel
  }

  resultArea.prefWidth <== (borderPane.width * 2) / 3
  demoOracleVBox.prefWidth <== (borderPane.width / 3)
  resultArea.prefHeight <== (borderPane.height * 2) / 3
  demoOracleVBox.prefHeight <== (borderPane.height * 2) / 3
  demoOracleArea.prefHeight <== demoOracleVBox.height * 0.9

  spaceRegion.prefWidth <== (borderPane.width - initButtonBar.width - acceptButtonBar.width - execButtonBar.width - 100) / 2
  spaceRegion2.prefWidth <== spaceRegion.prefWidth
  tableView.prefHeight <== borderPane.height / 3

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner
}
