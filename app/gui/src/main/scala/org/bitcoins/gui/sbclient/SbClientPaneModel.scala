package org.bitcoins.gui.sbclient

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.sbclient.{Exchange, TradingPair}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.gui.TaskRunner
import org.bitcoins.gui.dlc.GlobalDLCData
import org.bitcoins.gui.sbclient.dialog.{
  OpenChannelDialog,
  SbClientInvoiceDialog
}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.TextArea
import scalafx.stage.Window

import scala.util.{Failure, Success}

class SbClientPaneModel(resultArea: TextArea) {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  val oracleSigCaption = "Last Oracle Signature"
  val oracleInfoCaption = "Oracle Info"

  private def execCommand(
      caption: String,
      toCliCommand: (Exchange, TradingPair) => PriceDataApiCall): Unit = {
    val result =
      SbClientInvoiceDialog.showAndWait(caption, parentWindow.value)

    result match {
      case Some((exchangeStr, tradingPairStr)) =>
        val exchange = Exchange.fromString(exchangeStr).get
        val tradingPair = TradingPair.fromString(tradingPairStr)

        taskRunner.run(
          caption = s"$caption for $tradingPairStr on $exchangeStr",
          op = {

            val command = toCliCommand(exchange, tradingPair)
            ConsoleCli.exec(command, Config.empty) match {
              case Success(commandReturn) =>
                if (caption == oracleSigCaption) {
                  GlobalDLCData.lastOracleSig = commandReturn
                } else if (caption == oracleInfoCaption) {
                  GlobalDLCData.lastOracleInfo = commandReturn
                }
                resultArea.text = commandReturn
              case Failure(err) =>
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
          }
        )
      case None => ()
    }
  }

  def onOpenSbChannel(): Unit = {
    val result =
      OpenChannelDialog.showAndWait(parentWindow.value)

    result match {
      case Some(amount) =>
        val sats = Satoshis(amount.toLong)

        taskRunner.run(
          caption = s"Opening channel for $sats",
          op = {
            ConsoleCli.exec(OpenSbChannel(sats), Config.empty) match {
              case Success(commandReturn) =>
                resultArea.text = commandReturn
              case Failure(err) =>
                resultArea.text = s"Error executing command:\n${err.getMessage}"
                err.printStackTrace()
            }
          }
        )
      case None => ()
    }
  }

  def onPublicKey(): Unit = {
    execCommand("Public Key", GetSbPubKey.apply)
  }

  def onRValue(): Unit = {
    execCommand("R Value", GetSbRValue.apply)
  }

  def onOracleInfo(): Unit = {
    execCommand(oracleInfoCaption, GetSbOracleInfo.apply)
  }

  def onLastSig(): Unit = {
    execCommand(oracleSigCaption, GetSbLastSig.apply)
  }
}
