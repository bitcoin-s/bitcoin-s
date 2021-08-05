package org.bitcoins.gui

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.gui.dlc.DLCPane
import org.bitcoins.gui.util.GUIUtil
import scalafx.beans.property.StringProperty
import scalafx.geometry._
import scalafx.scene.control._
import scalafx.scene.layout._

abstract class WalletGUI extends Logging {

  def glassPane: VBox

  implicit def system: ActorSystem

  private lazy val statusLabel = new Label {
    maxWidth = Double.MaxValue
    text <== GlobalData.statusText
  }

  private lazy val infoLabel = new Label {
    text <== StringProperty("Sync Height: ") + GlobalData.syncHeight
  }

  private lazy val connectedLabel = new Label {
    text <== GlobalData.connectedStr
  }

  def fetchStartingData(): Unit = {
    model.startWalletInfoScheduler()
    model.updateFeeRate()
    dlcPane.model.setUp()
    model.updateTorAddress()
  }

  private val satsProperty = StringProperty(" sats")

  private lazy val confirmedText = new Label() {
    text <== StringProperty(
      "Confirmed balance:\t\t") + GlobalData.currentConfirmedBalance + satsProperty
  }

  private lazy val unconfirmedText = new Label() {
    text <== StringProperty(
      "Unconfirmed balance:\t") + GlobalData.currentUnconfirmedBalance + satsProperty
  }

  private lazy val reservedText = new Label() {
    text <== StringProperty(
      "Reserved balance:\t\t") + GlobalData.currentReservedBalance + satsProperty
  }

  private lazy val totalBalanceText = new Label() {
    text <== StringProperty(
      "Total balance:\t\t\t") + GlobalData.currentTotalBalance + satsProperty
  }

  private lazy val pnlText = new Label() {
    text <== StringProperty(
      "Profit and Loss:\t\t\t") + GlobalData.currentPNL + satsProperty
  }

  private lazy val rateOfReturnText = new Label() {
    text <== StringProperty("Rate of Return:\t\t\t") + GlobalData.rateOfReturn
  }

  private[gui] lazy val dlcPane = new DLCPane(glassPane)(system.dispatcher)
  private[gui] lazy val model = new WalletGUIModel(dlcPane.model)

  private lazy val balanceBox = new VBox {
    spacing = 10
    children = Vector(confirmedText,
                      unconfirmedText,
                      reservedText,
                      new Separator(),
                      totalBalanceText)
  }

  private lazy val walletAccountingBox = new VBox {
    spacing = 10
    children = Vector(pnlText, rateOfReturnText)
  }

  private lazy val getNewAddressButton = new Button {
    text = "Get New Address"
    onAction = _ => model.onGetNewAddress()
  }

  private lazy val sendButton = new Button {
    text = "Send"
    onAction = _ => model.onSend()
  }

  private lazy val spacer = new Region { vgrow = Priority.Always }

  private var nextRow: Int = 0

  private lazy val stateDetails = new GridPane {
    visible <== GlobalData.torAddress.isNotEmpty
    hgap = 5
    vgap = 5
    prefWidth = 300 // to match buttons
    columnConstraints = Seq(new ColumnConstraints { hgrow = Priority.Always },
                            new ColumnConstraints { hgrow = Priority.Always })

    val hbox = new HBox {
      alignment = Pos.Center
      children = Seq(
        new TextField {
          hgrow = Priority.Always
          text <== GlobalData.torAddress
        },
        GUIUtil.getCopyToClipboardButton(GlobalData.torAddress)
      )
    }

    add(new Label("Tor Address"), 0, nextRow)
    add(hbox, 1, nextRow)
    nextRow += 1
  }

  private lazy val sidebar = new VBox {
    padding = Insets(10)
    spacing = 20

    getNewAddressButton.prefWidth <== width
    sendButton.prefWidth <== width
    getNewAddressButton.maxWidth = 300
    sendButton.maxWidth = 300
    children = Vector(balanceBox,
                      walletAccountingBox,
                      getNewAddressButton,
                      sendButton,
                      spacer,
                      stateDetails)
  }

  lazy val bottomStack: HBox = new HBox {
    padding = Insets(5, 10, 5, 10)
    hgrow = Priority.Always
    children = Vector(statusLabel,
                      GUIUtil.getHSpacer(),
                      infoLabel,
                      GUIUtil.getHSpacer(),
                      connectedLabel)
  }

  lazy val borderPane: BorderPane = new BorderPane {
    top = AppMenuBar.menuBar(model)
    left = sidebar
    center = dlcPane.borderPane
    bottom = bottomStack
  }

  lazy val rootView: StackPane = new StackPane {
    children = Seq(
      borderPane,
      glassPane
    )
  }

  lazy val taskRunner: TaskRunner = {
    val runner = new TaskRunner(borderPane, glassPane)
    model.taskRunner = runner
    runner
  }
}
