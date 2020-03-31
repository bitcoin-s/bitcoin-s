package org.bitcoins.gui

import javafx.event.{ActionEvent, EventHandler}
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.StringProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}

object WalletGUI extends JFXApp {
  // Catch unhandled exceptions on FX Application thread
  Thread
    .currentThread()
    .setUncaughtExceptionHandler(
      new Thread.UncaughtExceptionHandler {
        override def uncaughtException(t: Thread, ex: Throwable): Unit = {
          ex.printStackTrace()
          val _ = new Alert(AlertType.Error) {
            initOwner(owner)
            title = "Unhandled exception"
            headerText = "Exception: " + ex.getClass + ""
            contentText = Option(ex.getMessage).getOrElse("")
          }.showAndWait()
        }
      }
    )

  private val glassPane = new VBox {
    children = new ProgressIndicator {
      progress = ProgressIndicator.IndeterminateProgress
      visible = true
    }
    alignment = Pos.Center
    visible = false
  }

  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
  }

  private val resultArea = new TextArea {
    editable = false
    wrapText = true
    text <== StringProperty("Your current balance is: ") + GlobalData.currentBalance + StringProperty(
      s"\n\n${(0 until 60).map(_ => "-").mkString("")}\n\n") + GlobalData.log
  }

  private val model = new WalletGUIModel()

  private val getNewAddressButton = new Button {
    text = "Get New Addreses"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onGetNewAddress()
    }
  }

  private val sendButton = new Button {
    text = "Send"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onSend()
    }
  }

  private val buttonBar = new HBox {
    children = Seq(getNewAddressButton, sendButton)
    alignment = Pos.Center
    spacing <== width / 2
  }

  private val borderPane = new BorderPane {
    top = buttonBar
    center = resultArea
    bottom = statusLabel
  }

  private val rootView = new StackPane {
    children = Seq(
      borderPane,
      glassPane
    )
  }

  stage = new JFXApp.PrimaryStage {
    title = "Bitcoin-S Wallet"
    scene = new Scene(rootView)
  }

  private val taskRunner = new TaskRunner(resultArea, glassPane, statusLabel)
  model.taskRunner = taskRunner

  Platform.runLater(sendButton.requestFocus())
}
