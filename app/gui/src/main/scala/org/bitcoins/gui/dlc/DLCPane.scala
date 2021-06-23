package org.bitcoins.gui.dlc

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.gui.TaskRunner
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import java.awt.Toolkit.getDefaultToolkit
import java.awt.datatransfer.StringSelection
import java.io.File
import java.nio.file.Files
import scala.concurrent.ExecutionContext
import scala.util.Properties

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
    buttons = Seq(acceptButton, broadcastDLCButton)
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
      val txtExtensionFilter = new ExtensionFilter("Text Files", "*.txt")
      val allExtensionFilter = new ExtensionFilter("All Files", "*")
      val fileChooser = new FileChooser() {
        extensionFilters.addAll(txtExtensionFilter, allExtensionFilter)
        selectedExtensionFilter = txtExtensionFilter
        initialDirectory = new File(Properties.userHome)
      }
      val chosenFile = fileChooser.showSaveDialog(null)
      Files.write(chosenFile.toPath, resultTextArea.text.value.getBytes)
      ()
    }
  }

  val copyResultButton: Button = new Button("Copy Result") {
    onAction = _ => {
      val clipboard = getDefaultToolkit.getSystemClipboard
      val sel = new StringSelection(resultTextArea.text.value)
      clipboard.setContents(sel, sel)
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

  resultArea.prefWidth <== borderPane.width
  resultArea.prefHeight <== (borderPane.height * 2) / 3

  tableView.prefHeight <== borderPane.height / 3

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner
}
