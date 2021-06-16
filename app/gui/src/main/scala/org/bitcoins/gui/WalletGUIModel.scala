package org.bitcoins.gui

import akka.actor.{ActorSystem, Cancellable}
import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.commons.serializers.PicklerKeys
import org.bitcoins.core.dlc.accounting.RateOfReturnUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.gui.dialog._
import org.bitcoins.gui.dlc.DLCPaneModel
import org.bitcoins.gui.util.GUIUtil
import scalafx.application.Platform
import scalafx.beans.property._
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, TextArea}
import scalafx.stage.Window

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}

class WalletGUIModel(dlcModel: DLCPaneModel)(implicit system: ActorSystem)
    extends Logging {
  val textArea: TextArea = dlcModel.resultArea
  var taskRunner: TaskRunner = _
  import system.dispatcher

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  lazy val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  private case object UpdateWalletInfoRunnable extends Runnable {

    override def run(): Unit = {
      Platform.runLater {
        updateBalance()
        updateWalletAccounting()
        updateWalletInfo()
        dlcModel.updateDLCs()
      }
    }
  }

  def startWalletInfoScheduler(): Cancellable = {
    system.scheduler.scheduleAtFixedRate(0.seconds, 10.seconds)(
      UpdateWalletInfoRunnable)
  }

  def updateFeeRate(): Try[FeeUnit] = {
    ConsoleCli.exec(EstimateFee, GlobalData.consoleCliConfig).map { feeStr =>
      val feeUnit = FeeUnit.fromString(feeStr)
      GlobalData.feeRate = feeUnit
      feeUnit
    }
  }

  def onGetNewAddress(): Unit = {
    val addressP = Promise[String]()

    taskRunner.run(
      caption = "Get New Address",
      op = {
        ConsoleCli.exec(GetNewAddress(None),
                        GlobalData.consoleCliConfig) match {
          case Success(commandReturn) => addressP.success(commandReturn)
          case Failure(err) =>
            addressP.failure(err)
            throw err
        }
      }
    )

    val address = Await.result(addressP.future, 15.seconds)
    GetNewAddressDialog.showAndWait(parentWindow.value, address)
  }

  def onSend(): Unit = {
    val result = SendDialog.showAndWait(parentWindow.value)

    result match {
      case Some(cmd) =>
        taskRunner.run(
          caption = s"Sending to ${cmd.destination}",
          op = {
            ConsoleCli.exec(cmd, GlobalData.consoleCliConfig) match {
              case Success(txid) =>
                if (txid.isEmpty) {
                  textArea.text = "Error, server did not return anything"
                } else {
                  textArea.text = s"Transaction sent! $txid"
                }
              case Failure(err) => throw err
            }
          }
        )
      case None => ()
    }

    updateBalance()
  }

  def onAbout(): Unit = {
    AboutDialog.showAndWait(parentWindow.value)
  }

  private def updateWalletInfo(): Unit = {
    ConsoleCli.exec(WalletInfo, GlobalData.consoleCliConfig) match {
      case Failure(_) => ()
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj("wallet").obj
        val height = json("height").num.toLong

        // Only update once we start syncing filters
        if (height != 0)
          GlobalData.syncHeight.value = height.toString
    }
  }

  private[gui] def updateBalance(): Unit = {
    ConsoleCli.exec(GetBalances(isSats = true),
                    GlobalData.consoleCliConfig) match {
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj
        val confirmedBalance =
          GUIUtil.numberFormatter.format(
            json("confirmed").str.split(' ').head.toLong)
        val unconfirmedBalance =
          GUIUtil.numberFormatter.format(
            json("unconfirmed").str.split(' ').head.toLong)
        val reservedBalance =
          GUIUtil.numberFormatter.format(
            json("reserved").str.split(' ').head.toLong)
        val totalBalance =
          GUIUtil.numberFormatter.format(
            json("total").str.split(' ').head.toLong)

        GlobalData.currentConfirmedBalance.value = confirmedBalance
        GlobalData.currentUnconfirmedBalance.value = unconfirmedBalance
        GlobalData.currentReservedBalance.value = reservedBalance
        GlobalData.currentTotalBalance.value = totalBalance
      case Failure(err) =>
        err.printStackTrace()
        val _ = new Alert(AlertType.Error) {
          initOwner(owner)
          title = "Could not retrieve wallet balance"
          headerText = s"Operation failed. Exception: ${err.getClass}"
          contentText = err.getMessage
        }.showAndWait()
    }
  }

  private def updateWalletAccounting(): Unit = {
    ConsoleCli.exec(GetDLCWalletAccounting, GlobalData.consoleCliConfig) match {
      case Failure(err) =>
        logger.error(s"Error fetching accounting", err)
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj
        val pnl = json(PicklerKeys.pnl).num.toLong.toString
        val rateOfReturn = json(PicklerKeys.rateOfReturn).num
        val rorPrettyPrint = RateOfReturnUtil.prettyPrint(rateOfReturn)
        GlobalData.currentPNL.value = pnl
        GlobalData.rateOfReturn.value = rorPrettyPrint
        ()
    }
  }
}
