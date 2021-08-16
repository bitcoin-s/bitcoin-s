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
import scodec.bits.ByteVector
import upickle.default._

import java.io.File
import java.nio.file.Files
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Properties, Success}

class DLCPaneModel(pane: DLCPane)(implicit ec: ExecutionContext)
    extends Logging {
  var taskRunner: TaskRunner = _

  val resultArea: TextArea = pane.resultTextArea

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
      pane.sortTable()
    }
    //purposely drop the future on the floor for now
    //as our GUI is not async safe at all
    ()
  }

  def updateDLCInList(status: DLCStatus): Unit = {
    val indexOpt = dlcs.zipWithIndex.find(_._1.dlcId == status.dlcId).map(_._2)

    indexOpt match {
      case Some(index) =>
        dlcs.update(index, status)
      case None =>
        dlcs += status
    }
  }

  def updateDLC(dlcId: Sha256Digest): Unit = {
    ConsoleCli.exec(GetDLC(dlcId), GlobalData.consoleCliConfig) match {
      case Failure(exception) => throw exception
      case Success(dlcStatus) =>
        val status = read[DLCStatus](dlcStatus)

        updateDLCInList(status)
        pane.sortTable()
    }
  }

  def updateDLCs(): Unit = {
    getDLCs.map { dlcs =>
      dlcs.foreach(updateDLCInList)
      val toRemove = GlobalDLCData.dlcs.diff(dlcs)
      GlobalDLCData.dlcs --= toRemove
      pane.sortTable()
    }

    //purposely drop the future on the floor for now
    //as our GUI code is not async safe at all
    ()
  }

  def printDLCDialogResult[T <: CliCommand](
      caption: String,
      dialog: DLCDialog[T],
      postProcessStr: String => String = str => str): String = {
    val result = dialog.showAndWait(parentWindow.value)

    val promise = Promise[String]()

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                resultArea.text = postProcessStr(commandReturn)
                promise.success(commandReturn)
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
                promise.success("")
            }
            updateDLCs()
          }
        )
      case None => promise.success("")
    }

    Await.result(promise.future, 15.seconds)
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

  def onExecute(): String = {
    printDLCDialogResult("ExecuteDLC", new ExecuteDLCDialog)
  }

  def onRefund(): String = {
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

  def rebroadcastFundingTx(contractId: ByteVector): Unit = {
    println("rebroadcastFundingTx " + contractId)
    taskRunner.run(
      "Rebroadcast Funding Tx",
      op = {
        ConsoleCli.exec(BroadcastDLCFundingTx(contractId),
                        GlobalData.consoleCliConfig) match {
          case Success(_) => {
            println("Rebroadcast Funding Tx Success")
            // TODO : Want to show Alert or success data in view
          }
          case Failure(err) => throw err
        }
      }
    )
  }

  def rebroadcastClosingTx(status: DLCStatus): Unit = {
    println("rebroadcastClosingTx " + status)
//    (DLCStatus.getContractId(status), getOracleSignatures(status)) match {
//      case (Some(contractId), Some(sigs)) =>
//        taskRunner.run(
//          "Rebroadcast Closing Tx",
//          op = {
//            // TODO : Where do these sigs come from?
//            println("Rebroadcasting " + contractId + " sigs: " + sigs)
//            ConsoleCli.exec(ExecuteDLC(contractId, sigs, false),
//                            GlobalData.consoleCliConfig) match {
//              case Success(_) => {
//                println("Rebroadcast Closing Tx Success")
//              }
//              case Failure(err) => throw err
//            }
//          }
//        )
//      case _ => // Nothing to do
//    }
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
