package org.bitcoins.gui.ptlc

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, ButtonBar, Label, TextArea}
import scalafx.scene.layout.{BorderPane, GridPane, Region, VBox}

class PTLCPane(glassPane: VBox) {
  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  private val resultArea = new TextArea {
    prefHeight = 750
    prefWidth = 1400
    editable = false
    text = "Click on Create or Accept to begin."
    wrapText = true
  }

  private val model = new PTLCPaneModel(resultArea)

  private val createButton = new Button {
    text = "Create"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onCreate()
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

  private val addSigButton = new Button {
    text = "Add Signature"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onAddSig()
    }
  }

  private val getPTLCButton = new Button {
    text = "Get PTLC"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onGetPTLC()
    }
  }

  private val broadcastPTLCButton = new Button {
    text = "Broadcast PTLC"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onBroadcastPTLC()
    }
  }

  private val claimButton = new Button {
    text = "Claim"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onClaim()
    }
  }

  private val refundButton = new Button {
    text = "Refund"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onRefund()
    }
  }

  private val getSecretButton = new Button {
    text = "Get Secret"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onGetSecret()
    }
  }

  private val initButtonBar = new ButtonBar {
    buttons = Seq(createButton, signButton)
  }

  private val acceptButtonBar = new ButtonBar {
    buttons =
      Seq(acceptButton, addSigButton, getPTLCButton, broadcastPTLCButton)
  }

  private val execButtonBar = new ButtonBar {
    buttons = Seq(claimButton, refundButton, getSecretButton)
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

  val borderPane: BorderPane = new BorderPane {
    top = buttonSpacer
    center = resultArea
    bottom = statusLabel
  }

  resultArea.prefWidth <== borderPane.width
  spaceRegion.prefWidth <== (borderPane.width - initButtonBar.width - acceptButtonBar.width - execButtonBar.width - 100) / 2
  spaceRegion2.prefWidth <== spaceRegion.prefWidth

  private val taskRunner = new TaskRunner(buttonSpacer, glassPane)
  model.taskRunner = taskRunner

  Platform.runLater(createButton.requestFocus())
}
