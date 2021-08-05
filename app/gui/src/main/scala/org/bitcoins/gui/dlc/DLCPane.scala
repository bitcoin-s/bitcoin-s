package org.bitcoins.gui.dlc

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.gui.TaskRunner
import org.bitcoins.gui.util.GUIUtil
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._

import scala.concurrent.ExecutionContext

class DLCPane(glassPane: VBox)(implicit ec: ExecutionContext) {

  val resultTextArea: TextArea = new TextArea {
    editable = false
    text =
      "Welcome to the Bitcoin-S DLC Wallet. To set up a new DLC, click Offer, if you have received an Offer, click Accept."
    wrapText = true
  }

  private val resultArea = new BorderPane() {
    padding = Insets(top = 10, right = 0, bottom = 10, left = 0)
    center = resultTextArea
    hgrow = Priority.Always
  }

  val model = new DLCPaneModel(this)

  private val offerButton = new Button {
    text = "Offer"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onOffer()
    }
    tooltip = Tooltip(
      "Initiates a DLC with the given oracle and contract parameters, generating an Offer message.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val acceptButton = new Button {
    text = "Accept"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onAccept()
    }
    tooltip = Tooltip("In response to an Offer, generates an Accept message.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val signButton = new Button {
    text = "Sign"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onSign()
    }
    tooltip = Tooltip("In response to an Accept, generates a Sign message.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val broadcastDLCButton = new Button {
    text = "Broadcast DLC"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onBroadcastDLC()
    }
    tooltip = Tooltip(
      "In response to a Sign, saves signatures and broadcasts the funding transaction.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val refundButton = new Button {
    text = "Refund"
    onAction = _ => {
      model.onRefund()
      ()
    }
    tooltip = Tooltip(
      "After the refund timeout, broadcasts the refund transaction to the blockchain.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val executeButton = new Button {
    text = "Execute"
    onAction = _ => {
      model.onExecute()
      ()
    }
    tooltip = Tooltip(
      "Given an oracle attestation, broadcasts the closing transaction to the blockchain.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val initButtonBar = new ButtonBar {
    buttons = Seq(offerButton, signButton)
  }

  private val acceptButtonBar = new ButtonBar {
    buttons = Seq(acceptButton, broadcastDLCButton)
  }

  private val execButtonBar = new ButtonBar {
    buttons = Seq(refundButton, executeButton)
  }

  private val buttonSpacer = new HBox {
    alignment = Pos.Center
    children = Vector(initButtonBar,
                      GUIUtil.getHSpacer(),
                      acceptButtonBar,
                      GUIUtil.getHSpacer(),
                      execButtonBar)
  }

  private val textAreaHBox = new HBox {
    children = Seq(resultArea)
    spacing = 10
  }

  val exportResultButton: Button = new Button("Export Result") {
    onAction = _ => {
      GUIUtil.showSaveDialog("Result", Some(resultTextArea.text.value), None)
    }
  }

  val copyResultButton: Button = new Button("Copy Result") {
    onAction = _ => {
      GUIUtil.setStringToClipboard(resultTextArea.text.value)
    }
  }

  val resultButtonHBox: HBox = new HBox() {
    spacing = 10
    children = Vector(exportResultButton, copyResultButton)
  }

  val tableView: TableView[DLCStatus] = new DLCTableView(model).tableView

  def sortTable(): Unit = tableView.sort()

  private val textAreasAndTableViewVBox = new VBox {
    children = Seq(textAreaHBox, resultButtonHBox, tableView)
    spacing = 10
  }

  val borderPane: BorderPane = new BorderPane {
    padding = Insets(top = 10, right = 10, bottom = 0, left = 10)
    top = buttonSpacer
    center = textAreasAndTableViewVBox
  }

  resultArea.prefHeight <== (borderPane.height * 2) / 3
  tableView.prefHeight <== borderPane.height / 3

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner
}
