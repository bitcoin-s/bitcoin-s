package org.bitcoins.gui

//import akka.actor.ActorSystem
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
  MenuItem,
  TableColumn,
  TableView,
  TextField
}
import scalafx.scene.layout._

import java.io.File
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Date
import scala.util.{Failure, Success}

class ContractGUI(glassPane: VBox) {

  def fetchStartingData(): Unit = {
    // Nothing to fetch for state yet...
  }

  private[gui] lazy val model = new ContractGUIModel()

  private lazy val addEventTF = new TextField {
    styleClass += "title-textfield"
    promptText = "New Event Hex"
    onKeyTyped = _ => {
      val event = model.addEvent(this.text.value.trim)
      event match {
        case Some(tup) =>
          clearEventTF()
          eventTableView.sort()
          // Set focus on new item
          eventTableView.getSelectionModel().select(tup)
          // Show view
          showCreateOfferPane(tup._1, tup._2)
        case None =>
      }
    }
  }

  private def clearEventTF(): Unit = {
    addEventTF.clear()
  }

  lazy val addEventHBox = new HBox {
    styleClass += "small"
    children = Seq(addEventTF)
  }

  private lazy val addContractTF = new TextField {
    styleClass += "title-textfield"
    promptText = "Contract Hex"
    onKeyTyped = _ => {
      val bool = onContractAdded(text.value.trim, None)
      if (bool) clearContractTF() // Clear on valid data
      ()
    }
  }

  private def clearContractTF(): Unit = {
    addContractTF.clear()
  }

  private lazy val fileChooserButton = GUIUtil.getFileChooserButton(file => {
    val hex = Files.readAllLines(file.toPath).get(0)
    val bool = onContractAdded(hex, Some(file))
    if (bool) addContractTF.clear()
  })

  lazy val addContractHBox = new HBox {
    styleClass += "small"
    children = Seq(fileChooserButton, addContractTF)
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
    var success = true
    // Accept / Sign / Broadcast identifier
    LnMessageFactory(DLCOfferTLV).fromHexT(hex) match { // Accept
      case Success(_) =>
        showAcceptOfferPane(hex)
      case Failure(_) =>
        LnMessageFactory(DLCAcceptTLV).fromHexT(hex) match { // Sign
          case Success(_) =>
            showSignDLCPane(hex, file)
          case Failure(_) =>
            LnMessageFactory(DLCSignTLV).fromHexT(hex) match { // Broadcast
              case Success(_) =>
                showBroadcastDLCPane(hex, file)
              case Failure(_) => success = false // Nothing else to check for
            }
        }
    }
    success
  }

  private lazy val contractStepPane: VBox = new VBox

  private def showContractStep(view: Parent): Unit = {
    contentDetailVBox.children = Seq()
    contractStepPane.children = Seq(view)
  }

  private lazy val contentDetailVBox = new VBox {
    alignment = Pos.Center
    hgrow = Priority.Always
  }

  lazy val contractViews = new VBox {
    margin = Insets(0, 0, 0, 4) // match sidebarAccordian Insets
    children = Seq(contractStepPane, contentDetailVBox)
  }

  def showDLCView(status: DLCStatus, model: DLCPaneModel): Unit = {
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
