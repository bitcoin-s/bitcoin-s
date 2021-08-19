package org.bitcoins.gui

import org.bitcoins.cli.CliCommand.{
  AcceptDLCCliCommand,
  AddDLCSigsAndBroadcastCliCommand,
  CreateDLCOffer,
  SignDLCCliCommand
}
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.tlv.{
  ContractInfoV0TLV,
  DLCAcceptTLV,
  DLCOfferTLV,
  DLCSignTLV,
  LnMessageFactory,
  OracleAnnouncementV0TLV
}
import org.bitcoins.gui.contract.GlobalContractData
import org.bitcoins.gui.dlc.DLCPaneModel
import org.bitcoins.gui.dlc.dialog.{
  AcceptOfferDialog,
  BroadcastDLCDialog,
  CreateDLCOfferDialog,
  DLCDialogContainer,
  SignDLCDialog,
  ViewDLCDialog
}
import org.bitcoins.gui.util.GUIUtil
import scalafx.beans.property.StringProperty
import scalafx.geometry._
import scalafx.scene.Parent
import scalafx.scene.control.{
  ContextMenu,
  Hyperlink,
  Label,
  MenuItem,
  TableColumn,
  TableView,
  TextArea,
  TextField
}
import scalafx.scene.layout._

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.util.{Failure, Success}

class ContractGUI(glassPane: VBox) {

  private[gui] lazy val model = new ContractGUIModel()

  private lazy val buildOfferLabel = new Label("Build Offer") {
    styleClass += "load-label"
  }

  private lazy val buildOfferTF = new TextField {
    styleClass += "load-textfield"
    promptText = "Paste Hex"
    onKeyTyped = _ => {
      val event = model.addEvent(this.text.value.trim)
      event match {
        case Some(tup) =>
          clearBuildOfferTF()
          eventTableView.sort()
          // Set focus on new item
          eventTableView.getSelectionModel().select(tup)
          // Show view
          showCreateOfferPane(tup._1, tup._2)
        case None =>
      }
    }
  }

  private def clearBuildOfferTF(): Unit = {
    buildOfferTF.clear()
  }

  private lazy val acceptLabel = new Label("Accept Offer") {
    styleClass += "load-label"
  }

  private lazy val acceptTF = new TextField {
    styleClass += "load-textfield"
    promptText = "Paste Hex"
    onKeyTyped = _ => {
      val validAddition = onContractAdded(text.value.trim, None)
      if (validAddition) clearAcceptTF() // Clear on valid data
      ()
    }
  }

  private def clearAcceptTF(): Unit = {
    acceptTF.clear()
  }

  lazy val loadPane = new GridPane {
    styleClass += "load-pane"
    padding = Insets(10, 0, 10, 11)
    hgap = 5
    add(buildOfferLabel, 0, 0)
    add(buildOfferTF, 1, 0)
    add(new Region { prefWidth = 57 }, 2, 0)
    add(acceptLabel, 3, 0)
    add(acceptTF, 4, 0)
  }

  private lazy val eventIdCol = new TableColumn[
    (OracleAnnouncementV0TLV, Option[ContractInfoV0TLV]),
    String] {
    text = "Event Id"
    prefWidth = 160
    cellValueFactory = { status =>
      val eventIdStr = status.value._1.eventTLV.eventId
      new StringProperty(status, "Event Id", eventIdStr)
    }
  }

  private val ISO_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'")

  private lazy val maturityCol = new TableColumn[
    (OracleAnnouncementV0TLV, Option[ContractInfoV0TLV]),
    String] {
    text = "Matures"
    prefWidth = 130
    cellValueFactory = { status =>
      val d = new Date(
        status.value._1.eventTLV.eventMaturityEpoch.toLong * 1000)
      new StringProperty(status, "Matures", ISO_FORMAT.format(d))
    }
  }

  private lazy val contractCol = new TableColumn[
    (OracleAnnouncementV0TLV, Option[ContractInfoV0TLV]),
    String] {
    text = "Contract"
    prefWidth = 160
    cellValueFactory = { status =>
      val contractStr =
        status.value._2 match {
          case None        => ""
          case Some(value) => value.contractDescriptor.toString()
        }
      new StringProperty(status, "Contract", contractStr)
    }
  }

  private lazy val eventTableView =
    new TableView[(OracleAnnouncementV0TLV, Option[ContractInfoV0TLV])](
      GlobalContractData.announcements) {
      columns ++= Seq(eventIdCol, maturityCol, contractCol)
      sortOrder.add(maturityCol)

      val removeContract: MenuItem = new MenuItem("Remove Contract") {
        onAction = _ => {
          val selected = selectionModel().getSelectedItem
          model.onRemoveContract(selected._1, selected._2)
        }
      }

      contextMenu = new ContextMenu() {
        items += removeContract
      }

      onMouseClicked = _ => {
        val i = selectionModel().getSelectedItem
        if (i != null) {
          showCreateOfferPane(i._1, i._2)
        }
      }
    }

  lazy val eventPane = new VBox {
    padding = Insets(0)
    children = Seq(eventTableView)
  }

  // Text paste / file browse handler
  private def onContractAdded(hex: String, file: Option[File]): Boolean = {
    // Accept / Sign / Broadcast identifier
    LnMessageFactory(DLCOfferTLV).fromHexT(hex) match { // Accept
      case Success(_) =>
        showAcceptOfferPane(hex)
        true
      case Failure(_) =>
        LnMessageFactory(DLCAcceptTLV).fromHexT(hex) match { // Sign
          case Success(_) =>
            showSignDLCPane(hex, file)
            true
          case Failure(_) =>
            LnMessageFactory(DLCSignTLV).fromHexT(hex) match { // Broadcast
              case Success(_) =>
                showBroadcastDLCPane(hex, file)
                true
              case Failure(_) => false // Nothing else to check for
            }
        }
    }
  }

  private lazy val contractStepPane: VBox = new VBox

  private def showContractStep(view: Parent): Unit = {
    resetContractViews()
    contentDetailVBox.children = Seq()
    contractStepPane.children = Seq(view)
  }

  private lazy val contentDetailVBox = new VBox {
    alignment = Pos.Center
    hgrow = Priority.Always
  }

  private lazy val welcomePane = new VBox {
    padding = Insets(15, 0, 0, 0)
    alignment = Pos.Center
    spacing = 15
    children = Seq(
      new Label("Welcome to Bitcoin-S!") {
        styleClass += "welcome-header"
      },
      new TextArea {
        styleClass += "welcome-textarea"
        minWidth = 250
        maxWidth = Double.MaxValue
        minHeight = 350
        // maxHeight doesn't seem to work here...
        text = "Paste hex code from an Announcement or Contract Template into Build Offer to start a new DLC Offer.\n\n" +
          "Give the Tor DLC Host Address in the lower left to your counterparty to use when accepting your Offer.\n\n" +
          "Paste hex code from an Offer you've received into Accept Offer to view and accept.\n\n" +
          "You can view DLC Contract details, Execute, Refund, and Rebroadcast transactions by selecting a Contract.\n\n" +
          "Individual DLC Operation dialogs are available from the DLC Operations window in the View menu.\n\n" +
          "You can backup Bitcoin-S from Save Backup in the File menu."
        wrapText = true
      },
      new Label("Links") { styleClass += "welcome-font" },
      new VBox {
        styleClass = Seq("link-background", "welcome-font")
        children = Seq(
          new Hyperlink("Suredbits Slack") {
            onAction = _ => GUIUtil.openUrl("https://suredbits.slack.com/")
          },
          new Hyperlink("Suredbits Oracle Explorer") {
            onAction = _ => GUIUtil.openUrl("https://oracle.suredbits.com/")
          },
          new Hyperlink("Be your own Oracle with Krystal Bull") {
            onAction = _ => GUIUtil.openUrl("https://suredbits.com/krystalbull")
          },
          new Hyperlink("Bitcoin-S Code Repository") {
            onAction =
              _ => GUIUtil.openUrl("https://github.com/bitcoin-s/bitcoin-s")
          }
        )
      }
    )
  }

  lazy val contractViews = new VBox {
    margin = Insets(0, 0, 0, 4) // match sidebarAccordian Insets
    children = Seq(welcomePane, contractStepPane, contentDetailVBox)
  }

  private def resetContractViews(): Unit = {
    contractViews.children = Seq(contractStepPane, contentDetailVBox)
  }

  def showDLCView(status: DLCStatus, model: DLCPaneModel): Unit = {
    resetContractViews()
    contractStepPane.children = Seq()
    contentDetailVBox.children = Seq(ViewDLCDialog.buildView(status, model))
  }

  private def showCreateOfferPane(
      announcement: OracleAnnouncementV0TLV,
      contractInfoOpt: Option[ContractInfoV0TLV]): Unit = {
    val offerDialog = new CreateDLCOfferDialog()
    val view = offerDialog.buildView(Some(announcement), contractInfoOpt)
    val container =
      new DLCDialogContainer[CreateDLCOffer]("New Offer",
                                             view,
                                             offerDialog,
                                             model.taskRunner,
                                             "offer")
    showContractStep(container.view)
  }

  private def showAcceptOfferPane(hex: String): Unit = {
    val dialog = new AcceptOfferDialog()
    val container =
      // AcceptDLCOffer -> AcceptDLCCliCommand
      new DLCDialogContainer[AcceptDLCCliCommand]("Accept Offer",
                                                  dialog.buildView(hex),
                                                  dialog,
                                                  model.taskRunner,
                                                  "accepted")
    showContractStep(container.view)
  }

  private def showSignDLCPane(hex: String, file: Option[File]): Unit = {
    val container =
      new DLCDialogContainer[SignDLCCliCommand](
        "Sign DLC",
        SignDLCDialog.buildView(hex, file),
        SignDLCDialog,
        model.taskRunner,
        "signed")
    container.toFileButton.visible = false
    container.toClipboardButton.visible = false
    showContractStep(container.view)
  }

  private def showBroadcastDLCPane(hex: String, file: Option[File]): Unit = {
    val container =
      new DLCDialogContainer[AddDLCSigsAndBroadcastCliCommand](
        "Broadcast DLC",
        BroadcastDLCDialog.buildView(hex, file),
        BroadcastDLCDialog,
        model.taskRunner,
        "broadcast")
    showContractStep(container.view)
  }

  private val taskRunner = new TaskRunner(contractViews, glassPane)
  model.taskRunner = taskRunner
}
