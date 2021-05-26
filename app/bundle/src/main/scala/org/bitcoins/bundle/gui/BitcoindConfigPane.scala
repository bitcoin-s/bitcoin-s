package org.bitcoins.bundle.gui

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.gui.util.GUIUtil
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.server._
import scalafx.geometry._
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.TextAlignment

import scala.util.Try

class BitcoindConfigPane(
    appConfig: BitcoinSAppConfig,
    model: LandingPaneModel) {

  private val bitcoindExplainer: Label = new Label() {
    margin = Insets(10)
    text = "This will fetch block data from the Bitcoin Core RPC." +
      " This does not require Bitcoin-S to do an initial block download but" +
      " Bitcoin Core will need to be synced."
    maxWidth = 600
    wrapText = true
    textAlignment = TextAlignment.Center
  }

  private val hostTF: TextField = new TextField() {
    text = appConfig.rpcBind.toString
    minWidth = 300
  }

  private val portTF: TextField = new TextField() {
    text = appConfig.bitcoindRpcConf.rpcPort.toString
    minWidth = 300
  }
  GUIUtil.setNumericInput(portTF)

  private val rpcUserTF: TextField = new TextField() {
    text = Try(appConfig.rpcUser).getOrElse("")
    minWidth = 300
  }

  private val rpcPasswordTF: TextField = new TextField() {
    text = Try(appConfig.rpcPassword).getOrElse("")
    minWidth = 300
  }

  private val versionComboBox: ComboBox[BitcoindVersion] =
    new ComboBox[BitcoindVersion](BitcoindVersion.standard) {
      value = appConfig.versionOpt.getOrElse(BitcoindVersion.newest)
      minWidth = 300
    }

  private var nextRow: Int = 0

  val gridPane: GridPane = new GridPane() {
    hgap = 5
    vgap = 5
    padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
    alignment = Pos.TopCenter

    add(new Label("RPC Host"), 0, nextRow)
    add(hostTF, 1, nextRow)
    nextRow += 1

    add(new Label("RPC Port"), 0, nextRow)
    add(portTF, 1, nextRow)
    nextRow += 1

    add(new Label("RPC Username"), 0, nextRow)
    add(rpcUserTF, 1, nextRow)
    nextRow += 1

    add(new Label("RPC Password"), 0, nextRow)
    add(rpcPasswordTF, 1, nextRow)
    nextRow += 1

    add(new Label("Bitcoin Core Version"), 0, nextRow)
    add(versionComboBox, 1, nextRow)
    nextRow += 1
  }

  val launchButton: Button = new Button("Launch Wallet") {
    margin = Insets(top = 80, right = 0, bottom = 0, left = 0)
    onAction = _ => model.launchWallet(getConfig, appConfig)
  }

  val view: Node = new VBox() {
    children = Vector(bitcoindExplainer, gridPane, launchButton)
    spacing = 20
    alignment = Pos.TopCenter
  }

  def getConfig: Config = {
    val configStr =
      s"""
         |bitcoin-s.node.mode = bitcoind
         |bitcoin-s.bitcoind-rpc.isRemote = true
         |bitcoin-s.bitcoind-rpc.rpcbind = "${hostTF.text.value}"
         |bitcoin-s.bitcoind-rpc.rpcport = ${portTF.text.value}
         |bitcoin-s.bitcoind-rpc.rpcuser = "${rpcUserTF.text.value}"
         |bitcoin-s.bitcoind-rpc.rpcpassword = "${rpcPasswordTF.text.value}"
         |bitcoin-s.bitcoind-rpc.version = "${versionComboBox.getValue}"
         |""".stripMargin

    ConfigFactory.parseString(configStr)
  }
}
