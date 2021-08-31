package org.bitcoins.gui

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.gui.dlc.DLCPane
import org.bitcoins.gui.util.GUIUtil
import scalafx.beans.property.{StringProperty}
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

  private lazy val networkLabel = new Label {
    text <== GlobalData.networkString
  }

  private lazy val infoLabel = new Label {
    text <== StringProperty("Sync Height: ") + GlobalData.syncHeight
  }

  private lazy val torProxyLabel = new Label {
    text <== GlobalData.torProxyEnabled
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
  private[gui] lazy val contractGUI = new ContractGUI(glassPane)

  private lazy val getNewAddressButton = new Button {
    text = "Get New Address"
    onAction = _ => model.onGetNewAddress()
    hgrow = Priority.Always
    maxWidth = 240
  }

  private lazy val sendButton = new Button {
    text = "Send"
    onAction = _ => model.onSend()
    hgrow = Priority.Always
    maxWidth = 240
  }

  private lazy val buttonBox = new HBox {
    spacing = 10
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
    minWidth = 490 // avoid text/value compression from SplitPane
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
        minWidth = 110 // Don't compress numerics
      },
      new ColumnConstraints(),
      new ColumnConstraints() { // spacer column
        prefWidth = 30
      },
      new ColumnConstraints(),
      new ColumnConstraints() {
        prefWidth = 90
        minWidth = 90 // Don't compress numerics
      }
    )
  }

  private lazy val wallet = new VBox {
    padding = Insets(10)
    spacing = 10
    children = Vector(walletGrid, buttonBox)
  }

  private lazy val sidebarAccordian = new VBox {
    padding = Insets(4)

    val walletUI = new TitledPane {
      content = wallet
      text = "Wallet"
    }

    val eventUI = new TitledPane {
      content = contractGUI.eventPane
      text = "Events"
      expanded = false
    }

    val contractUI = new TitledPane {
      content = dlcPane.tableView
      text = "Contracts"
      dlcPane.tableView.onMouseClicked = _ => {
        val i = dlcPane.tableView.getSelectionModel.getSelectedItem
        if (i != null) {
          contractGUI.showDLCView(i, dlcPane.model)
        }
      }
    }

    children = Vector(
      contractGUI.loadPane,
      walletUI,
      contractUI,
      eventUI,
      GUIUtil.getVSpacer(),
      stateDetails
    )
  }

  private lazy val rightPaneContent: VBox = new VBox {
    children = Vector(contractGUI.contractViews)
  }

  private lazy val stateDetails = new GridPane {
    visible <== GlobalData.torDLCHostAddress.isNotEmpty
    padding = Insets(4, 0, 0, 0)
    hgap = 5
    vgap = 5
    columnConstraints = Seq(new ColumnConstraints { hgrow = Priority.Always },
                            new ColumnConstraints { hgrow = Priority.Always })

    val hbox = new HBox {
      alignment = Pos.Center
      children = Seq(
        new TextField {
          hgrow = Priority.Always
          text <== GlobalData.torDLCHostAddress
        },
        GUIUtil.getCopyToClipboardButton(GlobalData.torDLCHostAddress)
      )
    }
    nextRow = 0
    add(new Label("Tor DLC Host Address"), 0, nextRow)
    add(hbox, 1, nextRow)
    nextRow += 1
  }

  lazy val rightPane: ScrollPane = new ScrollPane {
    padding = Insets(4, 4, 4, 0) // There's native pad on the SplitPane divider
    styleClass = Seq("scroll-pane")
    fitToHeight = true
    fitToWidth = true
    minWidth = 270
    hbarPolicy = ScrollPane.ScrollBarPolicy.Never
    content = rightPaneContent
  }

  lazy val splitPane: SplitPane = new SplitPane {
    items ++= Seq(sidebarAccordian, rightPane)
  }

  lazy val bottomStack: HBox = new HBox {
    padding = Insets(5, 10, 5, 10)
    hgrow = Priority.Always
    spacing = 15
    children = Vector(statusLabel,
                      GUIUtil.getHSpacer(),
                      networkLabel,
                      infoLabel,
                      GUIUtil.getHSpacer(),
                      torProxyLabel,
                      connectedLabel)
  }

  lazy val borderPane: BorderPane = new BorderPane {
    top = AppMenuBar.menuBar(model, dlcPane)
    center = splitPane
    bottom = bottomStack
  }

  // BundleGUI overrides initial state
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
