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

  private val resultArea = new TextArea {
    prefHeight = 750
    prefWidth = 800
    editable = false
    text = "Click on Offer or Accept to begin."
    wrapText = true
  }

  private val demoOracleArea = new TextArea {
    prefHeight = 700
    prefWidth = 400
    editable = false
    text = "Click Create Contract Info button to get started"
    wrapText = true
  }

  private val numOutcomesTF = new TextField {
    promptText = "Number of Outcomes"
  }

  private val model =
    new DLCPaneModel(resultArea, demoOracleArea, numOutcomesTF)

  private val demoOracleButton = new Button {
    text = "Create Contract Info"
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

  private val initCloseButton = new Button {
    text = "Init Close"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onInitClose()
    }
  }

  private val acceptCloseButton = new Button {
    text = "Accept Close"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onAcceptClose()
    }
  }

  private val refundButton = new Button {
    text = "Refund"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onRefund()
    }
  }

  private val forceCloseButton = new Button {
    text = "Force Close"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onForceClose()
    }
  }

  private val punishButton = new Button {
    text = "Punish"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onPunish()
    }
  }

  private val initButtonBar = new ButtonBar {
    buttons = Seq(offerButton, signButton)
  }

  private val acceptButtonBar = new ButtonBar {
    buttons = Seq(acceptButton, addSigsButton, getFundingButton)
  }

  private val execButtonBar = new ButtonBar {
    buttons = Seq(initCloseButton,
                  acceptCloseButton,
                  refundButton,
                  forceCloseButton,
                  punishButton)
  }

  private val spaceRegion = new Region()
  private val spaceRegion2 = new Region()

  private val buttonSpacer = new GridPane {
    hgap = 10
    prefHeight = 50
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

  val borderPane: BorderPane = new BorderPane {
    top = buttonSpacer
    center = textAreaHBox
    bottom = statusLabel
  }

  resultArea.prefWidth <== (borderPane.width * 2) / 3
  demoOracleVBox.prefWidth <== (borderPane.width / 3)

  spaceRegion.prefWidth <== (borderPane.width - initButtonBar.width - acceptButtonBar.width - execButtonBar.width - 100) / 2
  spaceRegion2.prefWidth <== spaceRegion.prefWidth

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner
}
