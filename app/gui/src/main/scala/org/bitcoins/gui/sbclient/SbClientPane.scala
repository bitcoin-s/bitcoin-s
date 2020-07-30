package org.bitcoins.gui.sbclient

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label, TextArea}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

class SbClientPane(glassPane: VBox) {

  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  private val resultArea = new TextArea {
    editable = false
    text = ""
    wrapText = true
  }

  private val model = new SbClientPaneModel(resultArea)

  private val openSbChannelButton = new Button {
    text = "Open Channel to Suredbits"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onOpenSbChannel()
    }
  }

  private val getPubKeyButton = new Button {
    text = "Get Public Key"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onPublicKey()
    }
  }

  private val getRValueButton = new Button {
    text = "Get R Value"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onRValue()
    }
  }

  private val getOracleInfoButton = new Button {
    text = "Get Oracle Info"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onOracleInfo()
    }
  }

  private val getLastSignatureButton = new Button {
    text = "Get Last Signature"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onLastSig()
    }
  }

  private val execButtonBar = new HBox {
    children = Seq(openSbChannelButton,
                   getPubKeyButton,
                   getRValueButton,
                   getOracleInfoButton,
                   getLastSignatureButton)
    alignment = Pos.Center
    spacing <== width / 20
  }

  private val textAreaHBox = new HBox {
    children = Seq(resultArea)
    spacing = 10
  }

  val borderPane: BorderPane = new BorderPane {
    top = execButtonBar
    center = textAreaHBox
    bottom = statusLabel
  }

  resultArea.prefWidth <== borderPane.width

  private val taskRunner = new TaskRunner(borderPane, glassPane)
  model.taskRunner = taskRunner
}
