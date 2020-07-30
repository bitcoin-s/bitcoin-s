package org.bitcoins.gui

import org.bitcoins.cli.CliCommand.{GetBalance, GetNewAddress, SendToAddress}
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.gui.dialog.{GetNewAddressDialog, SendDialog}
import scalafx.beans.property.{ObjectProperty, StringProperty}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.Alert
import scalafx.stage.Window

import scala.util.{Failure, Success}

class WalletGUIModel() {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  updateBalance()

  def onGetNewAddress(): Unit = {
    val address = StringProperty("")

    taskRunner.run(
      caption = "Get New Address",
      op = {
        ConsoleCli.exec(GetNewAddress, Config.empty) match {
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
            ConsoleCli.exec(SendToAddress(BitcoinAddress.fromString(address),
                                          Bitcoins(BigDecimal(amount)),
                                          satoshisPerVirtualByte = None,
                                          noBroadcast = false),
                            Config.empty) match {
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

  def updateBalance(): Unit = {
    ConsoleCli.exec(GetBalance(isSats = false), Config.empty) match {
      case Success(commandReturn) =>
        GlobalData.currentBalance.value = commandReturn.split(' ').head.toDouble
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
