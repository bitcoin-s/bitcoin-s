package org.bitcoins.gui

import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.image.Image
import org.bitcoins.gui.dlc.DLCPane
import org.bitcoins.gui.sbclient.SbClientPane
import org.bitcoins.gui.settings.SettingsPane
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.StringProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.TabPane.TabClosingPolicy
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
    text <== GlobalData.statusText
  }

  private val resultArea = new TextArea {
    editable = false
    wrapText = true
    text <== StringProperty(
      "Your current balance is: ") + GlobalData.currentBalance + StringProperty(
      s"\n\n${(0 until 60).map(_ => "-").mkString("")}\n\n") + GlobalData.log
  }

  val model = new WalletGUIModel()

  private val getNewAddressButton = new Button {
    text = "Get New Address"
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

  private val updateBalanceButton = new Button {
    text = "Update Balance"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.updateBalance()
    }
  }

  private val buttonBar = new HBox {
    children = Seq(updateBalanceButton, getNewAddressButton, sendButton)
    alignment = Pos.Center
    spacing <== width / children.size
  }

  private val borderPane = new BorderPane {
    top = buttonBar
    center = resultArea
    bottom = statusLabel
  }

  private val dlcPane = new DLCPane(glassPane)

  private val sbClientPane = new SbClientPane(glassPane)

  private val settingsPane = new SettingsPane

  private val tabPane: TabPane = new TabPane {

    val walletTab: Tab = new Tab {
      text = "Wallet"
      content = borderPane
    }

    val dlcTab: Tab = new Tab {
      text = "DLC"
      content = dlcPane.borderPane
    }

    val sbClientTab: Tab = new Tab {
      text = "Sb Client"
      content = sbClientPane.borderPane
    }

    val settingsTab: Tab = new Tab {
      text = "Settings"
      content = settingsPane.view
    }

    tabs = Seq(walletTab, dlcTab, sbClientTab, settingsTab)

    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  private val rootView = new StackPane {
    children = Seq(
      tabPane,
      glassPane
    )
  }

  private val walletScene = new Scene {
    root = rootView
    stylesheets = GlobalData.currentStyleSheets
  }

  stage = new JFXApp.PrimaryStage {
    title = "Bitcoin-S Wallet"
    scene = walletScene
    icons.add(new Image("/icons/bitcoin-s.png"))
  }

  private val taskRunner = new TaskRunner(resultArea, glassPane)
  model.taskRunner = taskRunner

  Platform.runLater(sendButton.requestFocus())

  override def stopApp(): Unit = {
    sys.exit(0) // Kills the server if GUI is closed in AppBundle
  }
}
