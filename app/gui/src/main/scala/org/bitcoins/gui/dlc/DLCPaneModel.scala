package org.bitcoins.gui.dlc

import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.gui.dlc.dialog._
import org.bitcoins.gui.{TaskRunner, WalletGUI}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{TextArea, TextField}
import scalafx.stage.Window

import scala.util.{Failure, Success}

class DLCPaneModel(
    resultArea: TextArea,
    oracleInfoArea: TextArea,
    numOutcomesTF: TextField) {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  def printDLCDialogResult[T <: CliCommand](
      caption: String,
      dialog: DLCDialog[T]): Unit = {
    val result = dialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command, Config.empty) match {
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

  def onInitOracle(): Unit = {
    val numOutcomes = BigInt(numOutcomesTF.text()).toInt
    require(numOutcomes <= 10, "More than 10 outcomes not supported.")

    val result = InitOracleDialog.showAndWait(parentWindow.value, numOutcomes)

    result match {
      case Some((_, contractInfo)) =>
        val builder = new StringBuilder()

        builder.append("Outcome hashes and amounts in order of entry:\n")
        contractInfo.foreach {
          case (hash, amt) => builder.append(s"${hash.hex} - ${amt.toLong}\n")
        }
        builder.append(s"\nSerialized Contract Info:\n${contractInfo.hex}\n\n")

        GlobalDLCData.lastContractInfo = contractInfo.hex

        oracleInfoArea.text = builder.result()
      case None => ()
    }
  }

  def onOffer(): Unit = {
    printDLCDialogResult("CreateDLCOffer", OfferDLCDialog)
    WalletGUI.model.updateBalance()
  }

  def onAccept(): Unit = {
    printDLCDialogResult("AcceptDLCOffer", AcceptDLCDialog)
    WalletGUI.model.updateBalance()
  }

  def onSign(): Unit = {
    printDLCDialogResult("SignDLC", SignDLCDialog)
  }

  def onAddSigs(): Unit = {
    printDLCDialogResult("AddDLCSigs", AddSigsDLCDialog)
  }

  def onGetFunding(): Unit = {
    printDLCDialogResult("GetDLCFundingTx", GetFundingDLCDialog)
  }

  def onInitClose(): Unit = {
    printDLCDialogResult("InitDLCMutualClose", InitCloseDLCDialog)
  }

  def onAcceptClose(): Unit = {
    printDLCDialogResult("AcceptDLCMutualClose", AcceptCloseDLCDialog)
  }

  def onForceClose(): Unit = {
    printDLCDialogResult("ExecuteUnilateralDLC", ForceCloseDLCDialog)
    WalletGUI.model.updateBalance()
  }

  def onPunish(): Unit = {
    printDLCDialogResult("ExecuteDLCPunishment", PunishDLCDialog)
  }

  def onRefund(): Unit = {
    printDLCDialogResult("ExecuteDLCRefund", RefundDLCDialog)
  }
}
