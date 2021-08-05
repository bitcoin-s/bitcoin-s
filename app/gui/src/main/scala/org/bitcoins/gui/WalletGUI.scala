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

  private[gui] lazy val dlcPane = new DLCPane(glassPane)(system.dispatcher)
  private[gui] lazy val model = new WalletGUIModel(dlcPane.model)

  private lazy val getNewAddressButton = new Button {
    text = "Get New Address"
    onAction = _ => model.onGetNewAddress()
  }

  private lazy val sendButton = new Button {
    text = "Send"
    onAction = _ => model.onSend()
  }

  private lazy val buttonBox = new HBox {
    spacing = 10
    getNewAddressButton.prefWidth <== width / 2
    sendButton.prefWidth <== width / 2
    getNewAddressButton.minWidth = 120
    sendButton.minWidth = 120
    getNewAddressButton.maxWidth = 240
    sendButton.maxWidth = 240
    children = Vector(getNewAddressButton, sendButton)
  }

  private var nextRow: Int = _

  private def getTextField(stringProperty: StringProperty): TextField =
    new TextField() {
      text <== stringProperty
      editable = false
      alignment = Pos.CenterRight
    }
  private def getSatsLabel(): Label = new Label("sats")

  private lazy val walletGrid = new GridPane() {
    minWidth = 490 // Matches button widths, this sets minWidth of sidebar
    styleClass += "no-text-input-readonly-style"
    nextRow = 0
    add(new Label("Confirmed Balance"), 0, nextRow)
    add(getTextField(GlobalData.currentConfirmedBalance), 1, nextRow)
    add(getSatsLabel(), 2, nextRow)
    nextRow += 1

    add(new Label("Unconfirmed Balance"), 0, nextRow)
    add(getTextField(GlobalData.currentUnconfirmedBalance), 1, nextRow)
    add(getSatsLabel(), 2, nextRow)
    nextRow += 1

    add(new Label("Reserve Balance"), 0, nextRow)
    add(getTextField(GlobalData.currentReservedBalance), 1, nextRow)
    add(getSatsLabel(), 2, nextRow)
    nextRow += 1

    add(new Separator(), 1, nextRow)
    nextRow += 1

    add(new Label("Total Balance"), 0, nextRow)
    add(getTextField(GlobalData.currentTotalBalance), 1, nextRow)
    add(getSatsLabel(), 2, nextRow)

    nextRow = 0 // 2nd column
    add(new Label("Profit and Loss"), 4, nextRow)
    add(getTextField(GlobalData.currentPNL), 5, nextRow)
    add(getSatsLabel(), 6, nextRow)
    nextRow += 1

    add(new Label("Rate of Return"), 4, nextRow)
    add(getTextField(GlobalData.rateOfReturn), 5, nextRow)
    add(new Label("%"), 6, nextRow)
    nextRow += 1

    // Force spacer column width
    columnConstraints = Seq(
      new ColumnConstraints(),
      new ColumnConstraints() {
        prefWidth = 110
      },
      new ColumnConstraints(),
      new ColumnConstraints() { // spacer column
        prefWidth = 30
      },
      new ColumnConstraints(),
      new ColumnConstraints() {
        prefWidth = 90
      }
    )
  }

  private lazy val wallet = new VBox {
    padding = Insets(10)
    spacing = 10
    children = Vector(walletGrid, buttonBox)
  }

  private lazy val stateDetails = new GridPane {
    visible <== GlobalData.torAddress.isNotEmpty
    hgap = 5
    vgap = 5
    prefWidth = 490 // to match wallet
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
    nextRow = 0
    add(new Label("Tor Address"), 0, nextRow)
    add(hbox, 1, nextRow)
    nextRow += 1
  }

  private lazy val sidebar = new VBox {
    padding = Insets(10)
    spacing = 20

    getNewAddressButton.prefWidth <== width
    sendButton.prefWidth <== width
    getNewAddressButton.maxWidth = 240
    sendButton.maxWidth = 240
    children = Vector(wallet, GUIUtil.getVSpacer(), stateDetails)
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
