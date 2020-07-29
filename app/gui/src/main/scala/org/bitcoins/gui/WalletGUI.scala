package org.bitcoins.gui

import javafx.scene.image.Image
import org.bitcoins.cli.CliCommand.GetInfo
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import org.bitcoins.gui.dlc.DLCPane
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, StackPane, VBox}

import scala.util.{Failure, Success}

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

  private val argsWithIndex = parameters.raw.zipWithIndex

  val rpcPortOpt: Option[Int] = {
    val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
    portOpt.map { case (_, idx) =>
      parameters.raw(idx + 1).toInt
    }
  }

  GlobalData.rpcPortOpt = rpcPortOpt

  val debug: Boolean = {
    parameters.raw.exists(_.toLowerCase == "--debug")
  }

  GlobalData.debug = debug

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

  private val confirmedText = new Label() {
    text <== StringProperty(
      "Confirmed balance:\t\t") + GlobalData.currentConfirmedBalance + StringProperty(
      " sats")
  }

  private val unconfirmedText = new Label() {
    text <== StringProperty(
      "Unconfirmed balance:\t") + GlobalData.currentReservedBalance + StringProperty(
      " sats")
  }

  private val reservedText = new Label() {
    text <== StringProperty(
      "Reserved balance:\t\t") + GlobalData.currentReservedBalance + StringProperty(
      " sats")
  }

  private val totalBalanceText = new Label() {
    text <== StringProperty(
      "Total balance:\t\t\t") + GlobalData.currentTotalBalance + StringProperty(
      " sats")
  }

  private val model = new WalletGUIModel()
  private val dlcPane = new DLCPane(glassPane)

  private val balanceBox = new VBox {
    spacing = 10
    children = Vector(confirmedText,
                      unconfirmedText,
                      reservedText,
                      new Separator(),
                      totalBalanceText)
  }

  private val getNewAddressButton = new Button {
    text = "Get New Address"
    onAction = _ => model.onGetNewAddress()
  }

  private val sendButton = new Button {
    text = "Send"
    onAction = _ => model.onSend()
  }

  private val sidebar = new VBox {
    padding = Insets(10)
    spacing = 20

    getNewAddressButton.prefWidth <== width
    sendButton.prefWidth <== width
    getNewAddressButton.maxWidth = 300
    sendButton.maxWidth = 300
    children = Vector(balanceBox, getNewAddressButton, sendButton)
  }

  private val borderPane = new BorderPane {
    top = AppMenuBar.menuBar(model)
    left = sidebar
    center = dlcPane.borderPane
    bottom = statusLabel
  }

  private val rootView = new StackPane {
    children = Seq(
      borderPane,
      glassPane
    )
  }

  private val walletScene = new Scene(1400, 600) {
    root = rootView
    stylesheets = GlobalData.currentStyleSheets
  }

  val info: BitcoinSServerInfo =
    ConsoleCli.exec(GetInfo, GlobalData.consoleCliConfig) match {
      case Failure(exception) =>
        throw exception
      case Success(str) =>
        val json = ujson.read(str)
        BitcoinSServerInfo.fromJson(json)
    }

  GlobalData.network = info.network

  val (img, titleStr): (Image, String) = info.network match {
    case MainNet =>
      (new Image("/icons/bitcoin-s.png"), "Bitcoin-S Wallet")
    case TestNet3 =>
      (new Image("/icons/bitcoin-s-testnet.png"),
       "Bitcoin-S Wallet - [testnet]")
    case RegTest =>
      (new Image("/icons/bitcoin-s-regtest.png"),
       "Bitcoin-S Wallet - [regtest]")
    case SigNet =>
      (new Image("/icons/bitcoin-s-signet.png"), "Bitcoin-S Wallet - [signet]")
  }

  stage = new JFXApp.PrimaryStage {
    title = titleStr
    scene = walletScene
    icons.add(img)
  }

  private val taskRunner = new TaskRunner(borderPane, glassPane)
  model.taskRunner = taskRunner

  override def stopApp(): Unit = {
    sys.exit(0) // Kills the server if GUI is closed in AppBundle
  }
}
