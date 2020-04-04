package org.bitcoins.gui.ptlc

import org.bitcoins.cli.{CliCommand, ConsoleCli}
import org.bitcoins.gui.TaskRunner
import org.bitcoins.gui.ptlc.dialog._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.TextArea
import scalafx.stage.Window

import scala.util.{Failure, Success}

class PTLCPaneModel(resultArea: TextArea) {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  def printPTLCDialogResult[T <: CliCommand](
      caption: String,
      dialog: PTLCDialog[T]): Unit = {
    val result = dialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command) match {
              case Success(commandReturn) => resultArea.text = commandReturn
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
          }
        )
      case None => ()
    }
  }

  def onCreate(): Unit = {
    printPTLCDialogResult("Create PTLC Invoice", CreatePTLCDialog)
  }

  def onAccept(): Unit = {
    printPTLCDialogResult("Accept PTLC Invoice", AcceptPTLCDialog)
  }

  def onSign(): Unit = {
    printPTLCDialogResult("Sign PTLC Refund", SignPTLCDialog)
  }

  def onAddSig(): Unit = {
    printPTLCDialogResult("Add PTLC Refund Signature", AddSigPTLCDialog)
  }

  def onGetPTLC(): Unit = {
    printPTLCDialogResult("Get PTLC", GetPTLCDialog)
  }

  def onBroadcastPTLC(): Unit = {
    printPTLCDialogResult("Broadcast PTLC", BroadcastPTLCDialog)
  }

  def onClaim(): Unit = {
    printPTLCDialogResult("Claim PTLC", ClaimPTLCDialog)
  }

  def onRefund(): Unit = {
    printPTLCDialogResult("Refund PTLC", RefundPTLCDialog)
  }

  def onGetSecret(): Unit = {
    printPTLCDialogResult("Get PTLC Secret", GetPTLCSecretDialog)
  }
}
