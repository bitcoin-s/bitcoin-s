package org.bitcoins.gui.dlc

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._

class DLCPane(glassPane: VBox) {

  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  private val resultTextArea = new TextArea {
    editable = false
    text = "Click on Offer or Accept to begin."
    wrapText = true
  }

  private val exportButton = new Button("Export Result") {
    alignmentInParent = Pos.BottomLeft
    onAction = _ => model.exportResult(resultTextArea.text.value)
  }

  private val resultArea = new BorderPane() {
    padding = Insets(10)
    center = resultTextArea
    bottom = exportButton
  }

  BorderPane.setMargin(exportButton, Insets(10))

  private val demoOracleArea = new TextArea {
    editable = false
    text =
      "Click on Init Demo Oracle to generate example oracle and contract information"
    wrapText = true
  }

  private val model =
    new DLCPaneModel(resultTextArea, demoOracleArea)

  // This is commented out because it will cause the GUI startup to fail on master
  // It should be uncommented when on the adaptor-dlc branch
  model.setUp()

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
      override def handle(event: ActionEvent): Unit = model.onExecute()
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
    children = Seq(resultArea)
    spacing = 10
  }

  private val tableView = new DLCTableView(model).tableView

  private val textAreasAndTableViewVBox = new VBox {
    children = Seq(textAreaHBox, tableView)
    spacing = 10
  }

  val borderPane: BorderPane = new BorderPane {
    top = buttonSpacer
    center = textAreasAndTableViewVBox
    bottom = statusLabel
  }

  resultArea.prefWidth <== borderPane.width
  resultArea.prefHeight <== (borderPane.height * 2) / 3

  spaceRegion.prefWidth <== (borderPane.width - initButtonBar.width - acceptButtonBar.width - execButtonBar.width - 100) / 2
  spaceRegion2.prefWidth <== spaceRegion.prefWidth
  tableView.prefHeight <== borderPane.height / 3

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner
}
