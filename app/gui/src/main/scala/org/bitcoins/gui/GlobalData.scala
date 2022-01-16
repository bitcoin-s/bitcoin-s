package org.bitcoins.gui

import org.bitcoins.cli.Config
import org.bitcoins.core.config._
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.gui.settings.Themes
import scalafx.beans.property._

object GlobalData {
  val currentConfirmedBalance: StringProperty = StringProperty("0")
  val currentUnconfirmedBalance: StringProperty = StringProperty("0")
  val currentReservedBalance: StringProperty = StringProperty("0")
  val currentTotalBalance: StringProperty = StringProperty("0")

  val currentPNL: StringProperty = StringProperty("0")
  val rateOfReturn: StringProperty = StringProperty("0")

  val syncHeight: StringProperty = StringProperty("Syncing headers...")

  private val torProxyEnabledStr = "● Tor Proxy"

  var network: BitcoinNetwork = _
  val torProxyEnabled = StringProperty("")
  val networkString: StringProperty = new StringProperty("")

  def setBitcoinNetwork(
      network: BitcoinNetwork,
      proxyEnabled: Boolean): Unit = {
    this.network = network
    networkString.value = "Network: " + network
    // Only showing Tor Proxy status when enabled
    if (proxyEnabled) torProxyEnabled.value = torProxyEnabledStr
  }

  val statusText: StringProperty = StringProperty("")

  private val isConnectedStr = "● Connected"
  private val isDisconnectedStr = "○ Disconnected"

  val connected: BooleanProperty = BooleanProperty(true)

  connected.addListener { (_, _, newValue) =>
    if (newValue) {
      connectedStr.value = isConnectedStr
    } else {
      connectedStr.value = isDisconnectedStr
    }
  }

  val connectedStr: StringProperty = StringProperty(isConnectedStr)

  var darkThemeEnabled: Boolean = true

  def currentStyleSheets: Seq[String] = {
    if (GlobalData.darkThemeEnabled) {
      Seq(Themes.DarkTheme.fileLocation)
    } else {
      Seq.empty
    }
  }

  var rpcPortOpt: Option[Int] = None

  var debug = false

  private var passwordOpt: Option[String] = None

  /** Sets the rpc password for the GUI */
  def setPassword(password: String): Unit = {
    passwordOpt = Some(password)
  }

  def consoleCliConfig: Config = {
    val rpcConfigAndDebug = rpcPortOpt match {
      case None =>
        Config(debug = debug)
      case Some(rpcPort) =>
        Config(debug = debug, rpcPortOpt = Some(rpcPort))
    }

    val passwordConfig = passwordOpt match {
      case Some(password) =>
        rpcConfigAndDebug.copy(rpcPassword = password)
      case None =>
        rpcConfigAndDebug
    }
    passwordConfig
  }

  lazy val broadcastUrl: String = GlobalData.network match {
    case MainNet =>
      "https://blockstream.info/api/tx"
    case TestNet3 =>
      "https://blockstream.info/testnet/api/tx"
    case net @ (RegTest | SigNet) => s"Broadcast from your own node on $net"
  }

  /** Builds a url for the Blockstream Explorer to view the tx */
  def buildBlockstreamExplorerTxUrl(txIdHex: String): String = {
    network match {
      case MainNet =>
        s"https://blockstream.info/tx/${txIdHex}"
      case TestNet3 =>
        s"https://blockstream.info/testnet/tx/${txIdHex}"
      case net @ (RegTest | SigNet) =>
        s"View transaction on your own node on $net"
    }
  }

  /** Builds a url for the mempool.space to view the tx */
  def buildMempoolSpaceTxUrl(txIdHex: String): String = {
    network match {
      case MainNet =>
        s"https://mempool.space/tx/${txIdHex}"
      case TestNet3 =>
        s"https://mempool.space/testnet/tx/${txIdHex}"
      case net @ RegTest =>
        s"View transaction on your own node on $net"
      case SigNet =>
        s"https://mempool.space/signet/tx/${txIdHex}"
    }
  }

  /** Builds a url for the Oracle Explorer to view an Announcement */
  def buildAnnouncementUrl(announcementHash: String): String = {
    network match {
      case MainNet =>
        s"https://oracle.suredbits.com/announcement/${announcementHash}"
      case TestNet3 =>
        s"https://test.oracle.suredbits.com/announcement/${announcementHash}"
      case net @ (RegTest | SigNet) =>
        s"View transaction on your own node on $net"
    }
  }

  var feeRate: FeeUnit = SatoshisPerVirtualByte.fromLong(50)

  val torDLCHostAddress = StringProperty("")
}
