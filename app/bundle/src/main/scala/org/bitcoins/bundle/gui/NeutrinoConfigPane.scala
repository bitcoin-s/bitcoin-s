package org.bitcoins.bundle.gui

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig.toNodeConf
import scalafx.geometry._
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.TextAlignment

class NeutrinoConfigPane(
    appConfig: BitcoinSAppConfig,
    model: LandingPaneModel) {

  private val neutrinoExplainer: Label = new Label() {
    margin = Insets(10)
    text =
      "Neutrino syncing will have the Bitcoin-S wallet fetch block headers and neutrino filters" +
        " from the given peer. This requires downloading the entire history of filters" +
        " on first startup which can take an hour or two."
    maxWidth = 600
    wrapText = true
    textAlignment = TextAlignment.Center
  }

  val defaultPeer: String = {
    appConfig.network match {
      case MainNet          => "neutrino.suredbits.com"
      case TestNet3         => "neutrino.testnet3.suredbits.com"
      case RegTest | SigNet => "localhost"
    }
  }

  val startingPeerAddress: String = {
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

  private val peerAddressTF: TextField = new TextField() {
    text = startingPeerAddress
    minWidth = 300
  }

  private var nextRow: Int = 0

  val gridPane: GridPane = new GridPane() {
    hgap = 5
    vgap = 5
    padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
    alignment = Pos.TopCenter

    add(new Label("Peer Address"), 0, nextRow)
    add(peerAddressTF, 1, nextRow)
    nextRow += 1
  }

  val launchButton: Button = new Button("Launch Wallet") {
    margin = Insets(top = 180, right = 0, bottom = 0, left = 0)
    onAction = _ => model.launchWallet(getConfig, appConfig)
  }

  val view: Node = new VBox() {
    alignment = Pos.TopCenter
    children = Vector(neutrinoExplainer, gridPane, launchButton)
    spacing = 20
  }

  def getConfig: Config = {
    val configStr =
      s"""
         |bitcoin-s.node.mode = neutrino
         |bitcoin-s.node.peers = ["${peerAddressTF.text.value}"]
         |""".stripMargin

    ConfigFactory.parseString(configStr)
  }
}
