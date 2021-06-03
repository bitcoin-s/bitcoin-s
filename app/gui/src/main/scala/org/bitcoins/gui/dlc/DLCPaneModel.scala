package org.bitcoins.gui.dlc

import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{CliCommand, ConsoleCli}
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto._
import org.bitcoins.gui._
import org.bitcoins.gui.dlc.GlobalDLCData.dlcs
import org.bitcoins.gui.dlc.dialog._
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, TextArea}
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Window}
import upickle.default._

import java.io.File
import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Properties, Success}

class DLCPaneModel(val resultArea: TextArea)(implicit ec: ExecutionContext)
    extends Logging {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  def getDLCs: Future[Vector[DLCStatus]] = {
    FutureUtil.makeAsync[Vector[DLCStatus]] { () =>
      ConsoleCli.exec(GetDLCs, GlobalData.consoleCliConfig) match {
        case Failure(exception) => throw exception
        case Success(dlcsStr) =>
          ujson.read(dlcsStr).arr.map(read[DLCStatus](_)).toVector
      }
    }
  }

  def setUp(): Unit = {
    dlcs.clear()
    getDLCs.map { walletDlcs =>
      dlcs ++= walletDlcs
    }
    //purposely drop the future on the floor for now
    //as our GUI is not async safe at all
    ()
  }

  def updateDLC(dlcId: Sha256Digest): Unit = {
    ConsoleCli.exec(GetDLC(dlcId), GlobalData.consoleCliConfig) match {
      case Failure(exception) => throw exception
      case Success(dlcStatus) =>
        dlcs += read[DLCStatus](ujson.read(dlcStatus))
        dlcs.find(_.dlcId == dlcId).foreach(dlcs -= _)
    }
  }

  def updateDLCs(): Unit = {
    val newDLCsF = getDLCs
    val toAddF = newDLCsF.map(_.diff(dlcs))
    val toRemoveF = newDLCsF.map(dlcs.diff)
    val _ = for {
      toAdd <- toAddF
      toRemove <- toRemoveF
    } yield {
      dlcs ++= toAdd
      dlcs --= toRemove
      ()
    }

    //purposely drop the future on the floor for now
    //as our GUI code is not async safe at all
    ()
  }

  def printDLCDialogResult[T <: CliCommand](
      caption: String,
      dialog: DLCDialog[T],
      postProcessStr: String => String = str => str): Unit = {
    val result = dialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                resultArea.text = postProcessStr(commandReturn)
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onOffer(): Unit = {
    val result = new CreateDLCOfferDialog().showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = "Create DLC Offer",
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                resultArea.text = commandReturn
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onAccept(): Unit = {
    val result = new AcceptOfferDialog().showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = "Accept DLC Offer",
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                val string = if (commandReturn.isEmpty) {
                  "Accepting DLC Offer timed out! Try again in a bit."
                } else commandReturn

                resultArea.text = string
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onSign(): Unit = {
    val result = SignDLCDialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = "Sign DLC",
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                val string = if (commandReturn.isEmpty) {
                  "Signing DLC timed out! Try again in a bit."
                } else commandReturn
                resultArea.text = string
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onBroadcastDLC(): Unit = {
    val result = BroadcastDLCDialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = "Broadcast DLC",
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                val string = if (commandReturn.isEmpty) {
                  "Broadcasting DLC timed out! Try again in a bit."
                } else commandReturn
                resultArea.text = string
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onExecute(): Unit = {
    printDLCDialogResult("ExecuteDLC", new ExecuteDLCDialog)
  }

  def onRefund(): Unit = {
    printDLCDialogResult("ExecuteDLCRefund", new RefundDLCDialog)
  }

  def viewDLC(status: DLCStatus): Unit = {
    updateDLC(status.dlcId)
    val updatedStatus = dlcs.find(_.tempContractId == status.tempContractId)
    ViewDLCDialog.showAndWait(parentWindow.value,
                              updatedStatus.getOrElse(status),
                              this)
  }

  def cancelDLC(status: DLCStatus): Unit = {
    val eventId =
      status.oracleInfo.singleOracleInfos.head.announcement.eventTLV.eventId

    val confirmed = status.state match {
      case DLCState.Offered | DLCState.Accepted =>
        new Alert(AlertType.Confirmation) {
          initOwner(owner)
          headerText = "Confirm Canceling DLC"
          contentText =
            s"Are you sure you want to cancel this DLC for $eventId?\n" +
              "This cannot be undone."
        }.showAndWait() match {
          case Some(ButtonType.OK) => true
          case None | Some(_)      => false
        }
      case DLCState.Signed =>
        new Alert(AlertType.Confirmation) {
          initOwner(owner)
          headerText = "Confirm Unsafe Canceling DLC"
          contentText =
            "Danger! If your counter-party has received your sign message then they will be able to execute the DLC even if you cancel!\n"
          s"Are you sure you want to cancel this DLC for $eventId?\n" +
            "This cannot be undone.\n"
        }.showAndWait() match {
          case Some(ButtonType.OK) => true
          case None | Some(_)      => false
        }
      case DLCState.Broadcasted | DLCState.Confirmed | DLCState.Claimed |
          DLCState.RemoteClaimed | DLCState.Refunded =>
        new Alert(AlertType.Error) {
          initOwner(owner)
          headerText = "Failed to Cancel DLC"
          contentText = "Cannot cancel a DLC after it has been signed"
        }.showAndWait()
        false
    }

    if (confirmed) {
      taskRunner.run(
        caption = "Canceling DLC",
        op = {
          ConsoleCli.exec(CancelDLC(status.dlcId),
                          GlobalData.consoleCliConfig) match {
            case Success(_)   => ()
            case Failure(err) => throw err
          }
          updateDLCs()
          Platform.runLater(GUI.model.updateBalance())
        }
      )
    }
  }

  def exportResult(result: String): Unit = {
    val txtFilter = new ExtensionFilter("Text Files", "*.txt")
    val allExtensionFilter = new ExtensionFilter("All Files", "*")
    val fileChooser = new FileChooser() {
      extensionFilters.addAll(txtFilter, allExtensionFilter)
      selectedExtensionFilter = txtFilter
      initialDirectory = new File(Properties.userHome)
    }

    val selectedFile = fileChooser.showSaveDialog(null)

    taskRunner.run(
      "Export Result",
      op = {
        if (selectedFile != null) {
          val bytes = result.getBytes

          Files.write(selectedFile.toPath, bytes)
          ()
        }
      }
    )
  }
}
