package org.bitcoins.gui

import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.gui.dialog._
import org.bitcoins.gui.util.GUIUtil
import scalafx.application.Platform
import scalafx.beans.property.{ObjectProperty, StringProperty}
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.Window

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class WalletGUIModel()(implicit system: ActorSystem) {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  lazy val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  private case object UpdateWalletInfoRunnable extends Runnable {

    override def run(): Unit = {
      Platform.runLater {
        updateBalance()
      }
    }
  }

  def startWalletInfoScheduler(): Cancellable = {
    import system.dispatcher
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
    val address = StringProperty("")

    taskRunner.run(
      caption = "Get New Address",
      op = {
        ConsoleCli.exec(GetNewAddress(None),
                        GlobalData.consoleCliConfig) match {
          case Success(commandReturn) => address.value = commandReturn
          case Failure(err)           => throw err
        }
      }
    )

    GetNewAddressDialog.showAndWait(parentWindow.value, address)
  }

  def onSend(): Unit = {
    val result = SendDialog.showAndWait(parentWindow.value)

    result match {
      case Some((address, amount)) =>
        taskRunner.run(
          caption = s"Send $amount to $address",
          op = {
            val sats = Satoshis(amount.toLong)

            ConsoleCli.exec(
              SendToAddress(BitcoinAddress.fromString(address),
                            Bitcoins(sats),
                            satoshisPerVirtualByte = None,
                            noBroadcast = false),
              GlobalData.consoleCliConfig
            ) match {
              case Success(txid) =>
                GlobalData.log.value =
                  s"Sent $amount to $address in tx: $txid\n\n${GlobalData.log()}"
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

  private def updateBalance(): Unit = {
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
}
