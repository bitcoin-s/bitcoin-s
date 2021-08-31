package org.bitcoins.bundle.gui

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.commons.util.DatadirUtil
import org.bitcoins.core.config._
import org.bitcoins.gui.util.GUIUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig.toNodeConf
import scalafx.geometry._
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.{Font, TextAlignment}

class NeutrinoConfigPane(
    appConfig: BitcoinSAppConfig,
    model: LandingPaneModel) {

  private val neutrinoExplainer: Label = new Label {
    padding = Insets(20)
    text =
      "Neutrino syncing will have the Bitcoin-S wallet fetch block headers and neutrino filters" +
        " from the given peer. This requires downloading the entire history of filters" +
        " on first startup which can take an hour or two."
    font = new Font(16)
    maxWidth = 600
    wrapText = true
    textAlignment = TextAlignment.Center
  }

  private def defaultPeerForNetwork(network: BitcoinNetwork): String = {
    network match {
      case MainNet          => "neutrino.suredbits.com"
      case TestNet3         => "neutrino.testnet3.suredbits.com"
      case RegTest | SigNet => "localhost"
    }
  }

  private val defaultPeer: String = defaultPeerForNetwork(appConfig.network)

  private val startingPeerAddress: String = {
    appConfig.peers.headOption match {
      case Some(peer) =>
        // if we are using the default suredbits node
        if (peer.contains(".suredbits.com")) {
          defaultPeer
        } else {
          peer
        }
      case None => defaultPeer
    }
  }

  private val networkComboBox: ComboBox[BitcoinNetwork] =
    new ComboBox[BitcoinNetwork](
      BitcoinNetworks.knownNetworks.map(_.asInstanceOf[BitcoinNetwork])) {
      value = BitcoinNetworks.fromString(appConfig.chainConf.network.name)
      onAction = _ => {
        val peer = peerAddressTF.text.value
        if (
          peer.contains(".suredbits.com") || peer.contains("localhost") || peer
            .contains("127.0.0.1")
        ) {
          val network = selectionModel().getSelectedItem
          peerAddressTF.text.value = defaultPeerForNetwork(network)
        }
      }
      minWidth = 300
    }

  private val peerAddressTF: TextField = new TextField {
    text = startingPeerAddress
    minWidth = 300
  }

  private val torCheckBox: CheckBox = new CheckBox {
    selected = appConfig.nodeConf.socks5ProxyParams.isDefined
  }

  private var nextRow: Int = 0

  private val gridPane: GridPane = new GridPane {
    hgap = 10
    vgap = 10
    alignment = Pos.TopCenter

    add(new Label("Network"), 0, nextRow)
    add(networkComboBox, 1, nextRow)
    nextRow += 1
    add(new Label("Peer Address"), 0, nextRow)
    add(peerAddressTF, 1, nextRow)
    nextRow += 1

    add(new Label("Use Tor"), 0, nextRow)
    add(torCheckBox, 1, nextRow)
    nextRow += 1
  }

  private val launchButton: Button = new Button("Launch Wallet") {
    onAction = _ => model.launchWallet(getConfig, appConfig)
  }

  val view: Node = new VBox {
    spacing = 20
    alignment = Pos.TopCenter
    children =
      Vector(neutrinoExplainer, gridPane, GUIUtil.getVSpacer(), launchButton)
  }

  def getConfig: Config = {
    // Auto-enable proxy for .onion peers
    val proxyEnabled =
      torCheckBox.selected.value || peerAddressTF.text.value.contains(".onion")
    val configStr = s"""
                       |bitcoin-s.proxy.enabled = ${proxyEnabled}
                       |bitcoin-s.network = ${DatadirUtil.networkStrToDirName(
      networkComboBox.value.value.toString)}
                       |bitcoin-s.node.mode = neutrino
                       |bitcoin-s.node.peers = ["${peerAddressTF.text.value}"]
                       |""".stripMargin
    ConfigFactory.parseString(configStr)
  }
}
