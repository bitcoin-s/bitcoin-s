package org.bitcoins.gui.dlc

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.gui.TaskRunner
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._

import java.awt.Toolkit.getDefaultToolkit
import java.awt.datatransfer.StringSelection

class DLCPane(glassPane: VBox) {

  private val resultTextArea = new TextArea {
    editable = false
    text =
      "Welcome to the Bitcoin-S DLC Wallet. To set up a new DLC, click Offer, if you have received an Offer, click Accept."
    wrapText = true
  }

  private val resultArea = new BorderPane() {
    padding = Insets(10)
    center = resultTextArea
  }

  val model = new DLCPaneModel(resultTextArea)

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

  private val addSigsButton = new Button {
    text = "Add Sigs"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onAddSigs()
    }
    tooltip = Tooltip("In response to a Sign, saves signatures.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val getFundingButton = new Button {
    text = "Get Funding Tx"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onGetFunding()
    }
    tooltip = Tooltip(
      "After adding signatures, generates and broadcasts the DLC to the blockchain.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val refundButton = new Button {
    text = "Refund"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onRefund()
    }
    tooltip = Tooltip(
      "After the refund timeout, broadcasts the refund transaction to the blockchain.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  private val executeButton = new Button {
    text = "Execute"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onExecute()
    }
    tooltip = Tooltip(
      "Given an oracle attestation, broadcasts the closing transaction to the blockchain.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
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

  private val buttonSpacer = new HBox {
    spacing = 100
    alignment = Pos.Center
    children = Vector(initButtonBar, acceptButtonBar, execButtonBar)
  }

  private val textAreaHBox = new HBox {
    children = Seq(resultArea)
    spacing = 10
  }

  val exportResultButton: Button = new Button("Export Result") {
    onAction = _ => {
      val clipboard = getDefaultToolkit.getSystemClipboard
      val sel = new StringSelection(resultTextArea.text.value)
      clipboard.setContents(sel, sel)
    }
  }

  private val tableView = new DLCTableView(model).tableView

  private val textAreasAndTableViewVBox = new VBox {
    children = Seq(textAreaHBox, exportResultButton, tableView)
    spacing = 10
  }

  val borderPane: BorderPane = new BorderPane {
    padding = Insets(top = 10, right = 10, bottom = 0, left = 10)
    top = buttonSpacer
    center = textAreasAndTableViewVBox
  }

  resultArea.prefWidth <== borderPane.width
  resultArea.prefHeight <== (borderPane.height * 2) / 3

  tableView.prefHeight <== borderPane.height / 3

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner
}
