package org.bitcoins.gui.addresses

import org.bitcoins.cli.CliCommand.{GetAddresses, GetNewAddress}
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.gui.TaskRunner
import play.api.libs.json.Json
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer

import scala.util.{Failure, Success}

class AddressesPaneModel() {
  var taskRunner: TaskRunner = _

  val addresses: ObservableBuffer[BitcoinAddress] =
    new ObservableBuffer[BitcoinAddress]()

  var _selectedAddresses: ObservableBuffer[BitcoinAddress] = _
  def selectedAddresses: ObservableBuffer[BitcoinAddress] = _selectedAddresses

  def selectedAddresses_=(addresses: ObservableBuffer[BitcoinAddress]): Unit = {
    _selectedAddresses = addresses
  }

  private def getAddresses(): Vector[BitcoinAddress] = {
    import org.bitcoins.commons.serializers.JsonSerializers.bitcoinAddressReads

    ConsoleCli.exec(GetAddresses, Config.empty) match {
      case Success(addresses) =>
        Json.parse(addresses).validate[Vector[BitcoinAddress]].get
      case Failure(err) => throw err
    }
  }

  def setUp(): Unit = {
    addresses.clear()
    addresses ++= getAddresses()
  }

  def onGetNewAddress(): Unit = {
    taskRunner.run(
      caption = "Get New Address",
      op = {
        val _ = ConsoleCli.exec(GetNewAddress, Config.empty) match {
          case Success(commandReturn) =>
            BitcoinAddress.fromStringExn(commandReturn)
          case Failure(err) => throw err
        }
        val updatedAddresses = getAddresses()
        Platform.runLater(updateAddresses(updatedAddresses))
      }
    )
  }

  private def updateAddresses(
      updatedAddresses: Vector[BitcoinAddress]): Unit = {
    val toAdd = updatedAddresses.diff(addresses)
    val toRemove = addresses.diff(updatedAddresses)
    addresses ++= toAdd
    addresses --= toRemove
  }
}
