package org.bitcoins.bundle.gui

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.server.BitcoinSAppConfig
import scalafx.geometry._
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout._

class NeutrinoConfigPane(
    appConfig: BitcoinSAppConfig,
    model: LandingPaneModel) {

  private val peerAddressTF: TextField = new TextField() {
    text = appConfig.peers.headOption.getOrElse("")
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
    onAction = _ => model.launchWallet(getConfig, appConfig)
  }

  val view: Node = new VBox() {
    alignment = Pos.TopCenter
    children = Vector(gridPane, launchButton)
    spacing = 200
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
