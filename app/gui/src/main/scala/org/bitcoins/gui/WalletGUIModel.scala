package org.bitcoins.gui

import akka.actor.{ActorSystem, Cancellable}
import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.core.dlc.accounting.RateOfReturnUtil
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.gui.dialog._
import org.bitcoins.gui.dlc.DLCPaneModel
import org.bitcoins.gui.util.GUIUtil
import scalafx.application.Platform
import scalafx.beans.property._
import scalafx.scene.control.TextArea
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
        var success = updateBalance()
        success &= updateWalletAccounting()
        success &= updateWalletInfo()
        // updateDLCs is a future, we can't get if it succeeded or not
        // it should be caught by one of the others anyways
        dlcModel.updateDLCs()

        GlobalData.connected.value = success
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
                  Platform.runLater(
                    TransactionSentDialog.show(parentWindow.value, txid))
                }
              case Failure(err) => throw err
            }
          }
        )
      case None => ()
    }

    updateBalance()
    ()
  }

  def onAbout(): Unit = {
    AboutDialog.showAndWait(parentWindow.value)
  }

  def onDebug(): Unit = {
    DebugDialog.show(parentWindow.value)
  }

  /** Updates the wallet sync height
    * @return if the update was successful
    */
  private def updateWalletInfo(): Boolean = {
    ConsoleCli.exec(WalletInfo, GlobalData.consoleCliConfig) match {
      case Failure(_) => false
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj("wallet").obj
        val height = json("height").num.toLong

        // Only update once we start syncing filters
        if (height != 0)
          GlobalData.syncHeight.value = height.toString

        true
    }
  }

  /** Updates the wallet balances
    * @return if the update was successful
    */
  private[gui] def updateBalance(): Boolean = {
    ConsoleCli.exec(GetBalances(isSats = true),
                    GlobalData.consoleCliConfig) match {
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj
        val confirmedBalance =
          GUIUtil.numberFormatter.format(json("confirmed").num)
        val unconfirmedBalance =
          GUIUtil.numberFormatter.format(json("unconfirmed").num)
        val reservedBalance =
          GUIUtil.numberFormatter.format(json("reserved").num)
        val totalBalance =
          GUIUtil.numberFormatter.format(json("total").num)

        GlobalData.currentConfirmedBalance.value = confirmedBalance
        GlobalData.currentUnconfirmedBalance.value = unconfirmedBalance
        GlobalData.currentReservedBalance.value = reservedBalance
        GlobalData.currentTotalBalance.value = totalBalance
        true
      case Failure(err) =>
        err.printStackTrace()
        false
    }
  }

  /** Updates the wallet's DLC accounting details
    * @return if the update was successful
    */
  private def updateWalletAccounting(): Boolean = {
    ConsoleCli.exec(GetDLCWalletAccounting, GlobalData.consoleCliConfig) match {
      case Failure(err) =>
        logger.error(s"Error fetching accounting", err)
        false
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj
        val pnl = json(PicklerKeys.pnl).num.toLong
        val rateOfReturn = json(PicklerKeys.rateOfReturn).num
        val rorPercentage = RateOfReturnUtil.toPercentage(rateOfReturn)
        GlobalData.currentPNL.value = GUIUtil.numberFormatter.format(pnl)
        GlobalData.rateOfReturn.value = rorPercentage
        true
    }
  }

  // Address returned from GetDLCHostAddress when Tor is disabled
  private val DEFAULT_TOR_ADDRESS = "0:0:0:0:0:0:0:0:2862"

  /** Retrieves the tor endpoint address
    */
  def updateTorAddress(): Unit = {
    ConsoleCli.exec(GetDLCHostAddress, GlobalData.consoleCliConfig) match {
      case Failure(err) =>
        logger.error(s"Error fetching tor address", err)
      case Success(commandReturn) =>
        // Leave Tor Address out of UI if Tor is not enabled
        if (commandReturn != DEFAULT_TOR_ADDRESS)
          GlobalData.torDLCHostAddress.value = commandReturn
    }
  }
}
