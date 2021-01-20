package org.bitcoins.gui

import org.bitcoins.cli.Config
import org.bitcoins.core.config._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.gui.settings.Themes
import scalafx.beans.property.{LongProperty, StringProperty}

object GlobalData {
  val currentBalance: LongProperty = LongProperty(0)

  var network: BitcoinNetwork = _

  val log: StringProperty = StringProperty("")

  val statusText: StringProperty = StringProperty("")

  val darkThemeEnabled: Boolean = true

  def currentStyleSheets: Seq[String] =
    if (GlobalData.darkThemeEnabled) {
      Seq(Themes.DarkTheme.fileLocation)
    } else {
      Seq.empty
    }

  var rpcPortOpt: Option[Int] = None

  var debug = false

  def consoleCliConfig: Config =
    rpcPortOpt match {
      case None =>
        Config(debug = debug)
      case Some(rpcPort) =>
        Config(debug = debug, rpcPort = rpcPort)
    }

  lazy val broadcastUrl: String = GlobalData.network match {
    case MainNet =>
      "https://blockstream.info/api/tx"
    case TestNet3 =>
      "https://blockstream.info/testnet/api/tx"
    case net @ (RegTest | SigNet) => s"Broadcast from your own node on $net"
  }

  /** Builds a url for the blockstream explorer to view the tx */
  def buildTxUrl(txid: DoubleSha256DigestBE): String = {
    network match {
      case MainNet =>
        s"https://blockstream.info/tx/${txid.hex}"
      case TestNet3 =>
        s"https://blockstream.info/testnet/tx/${txid.hex}"
      case net @ (RegTest | SigNet) =>
        s"View transaction on your own node on $net"
    }
  }
}
