package org.bitcoins.bundle.gui

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.db.util.ServerArgParser
import org.bitcoins.gui._
import org.bitcoins.node.NodeType
import org.bitcoins.server.BitcoinSAppConfig
import scalafx.geometry._
import scalafx.scene.control.TabPane.TabClosingPolicy
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text._

import scala.util.Try

class LandingPane(glassPane: VBox, serverArgParser: ServerArgParser)(implicit
    system: ActorSystem,
    appConfig: BitcoinSAppConfig)
    extends Logging {

  val model = new LandingPaneModel(serverArgParser)

  private val label: Label = new Label("Welcome to Bitcoin-S") {
    alignmentInParent = Pos.BottomCenter
    textAlignment = TextAlignment.Center
    font = new Font(30)
  }

  val bitcoindConfigPane = new BitcoindConfigPane(appConfig, model)

  val bitcoindTab: Tab = new Tab() {
    text = "Bitcoin RPC Config"
    content = bitcoindConfigPane.view
  }

  val neutrinoConfigPane = new NeutrinoConfigPane(appConfig, model)

  val neutrinoTab: Tab = new Tab() {
    text = "Neutrino Config"
    content = neutrinoConfigPane.view
  }

  val isNeutrino: Boolean =
    Try(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode).getOrElse(false)

  val tabPane: TabPane = new TabPane() {
    tabs =
      if (isNeutrino) // if neutrino config, open at that tab
        Vector(neutrinoTab, bitcoindTab)
      else Vector(bitcoindTab, neutrinoTab)

    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  val view: BorderPane = new BorderPane {
    padding = Insets(top = 10, right = 10, bottom = 10, left = 10)

    top = label
    center = tabPane
  }

  view.autosize()

  val taskRunner: TaskRunner = {
    val runner = new TaskRunner(view, glassPane)
    model.taskRunner = runner
    runner
  }
}
