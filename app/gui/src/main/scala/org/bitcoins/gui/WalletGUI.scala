package org.bitcoins.gui

import akka.actor.ActorSystem
import org.bitcoins.gui.dlc.DLCPane
import scalafx.beans.property.StringProperty
import scalafx.geometry._
import scalafx.scene.control._
import scalafx.scene.layout._

abstract class WalletGUI {

  def glassPane: VBox

  implicit def system: ActorSystem

  private lazy val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  private lazy val infoLabel = new Label {
    padding = Insets(top = 0, right = 0, bottom = 10, left = 0)
    text <== StringProperty("Sync Height: ") + GlobalData.syncHeight
  }

  def fetchStartingData(): Unit = {
    model.startWalletInfoScheduler()
    model.updateFeeRate()
    dlcPane.model.setUp()
  }

  private lazy val confirmedText = new Label() {
    text <== StringProperty(
      "Confirmed balance:\t\t") + GlobalData.currentConfirmedBalance + StringProperty(
      " sats")
  }

  private lazy val unconfirmedText = new Label() {
    text <== StringProperty(
      "Unconfirmed balance:\t") + GlobalData.currentReservedBalance + StringProperty(
      " sats")
  }

  private lazy val reservedText = new Label() {
    text <== StringProperty(
      "Reserved balance:\t\t") + GlobalData.currentReservedBalance + StringProperty(
      " sats")
  }

  private lazy val totalBalanceText = new Label() {
    text <== StringProperty(
      "Total balance:\t\t\t") + GlobalData.currentTotalBalance + StringProperty(
      " sats")
  }

  private lazy val dlcPane = new DLCPane(glassPane)
  private lazy val model = new WalletGUIModel(dlcPane.model)

  private lazy val balanceBox = new VBox {
    spacing = 10
    children = Vector(confirmedText,
                      unconfirmedText,
                      reservedText,
                      new Separator(),
                      totalBalanceText)
  }

  private lazy val getNewAddressButton = new Button {
    text = "Get New Address"
    onAction = _ => model.onGetNewAddress()
  }

  private lazy val sendButton = new Button {
    text = "Send"
    onAction = _ => model.onSend()
  }

  private lazy val sidebar = new VBox {
    padding = Insets(10)
    spacing = 20

    getNewAddressButton.prefWidth <== width
    sendButton.prefWidth <== width
    getNewAddressButton.maxWidth = 300
    sendButton.maxWidth = 300
    children = Vector(balanceBox, getNewAddressButton, sendButton)
  }

  lazy val bottomStack: StackPane = new StackPane() {
    children = Vector(statusLabel, infoLabel)
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
