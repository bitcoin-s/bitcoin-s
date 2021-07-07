package org.bitcoins.bundle.gui

import org.bitcoins.bundle.util.BitcoinSAppJFX3
import org.bitcoins.db.util.{DatadirParser, ServerArgParser}
import org.bitcoins.gui._
import org.bitcoins.gui.util.GUIUtil
import org.bitcoins.server.BitcoinSAppConfig
import scalafx.application.{JFXApp3, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.VBox

object BundleGUI extends WalletGUI with BitcoinSAppJFX3 {

  override val customFinalDirOpt: Option[String] = None

  override val actorSystemName: String =
    s"bitcoin-s-gui-${System.currentTimeMillis()}"

  override lazy val commandLineArgs: Array[String] = parameters.raw.toArray

  override def start(): Unit = {
    // Catch unhandled exceptions on FX Application thread
    Thread
      .currentThread()
      .setUncaughtExceptionHandler((_: Thread, ex: Throwable) => {
        ex.printStackTrace()
        val _ = new Alert(AlertType.Error) {
          initOwner(owner)
          title = "Unhandled exception"
          headerText = "Exception: " + ex.getClass + ""
          contentText = Option(ex.getMessage).getOrElse("")
        }.showAndWait()
      })

    lazy val serverArgParser = ServerArgParser(commandLineArgs.toVector)

    val datadirParser = DatadirParser(serverArgParser, customFinalDirOpt)

    System.setProperty("bitcoins.log.location",
                       datadirParser.networkDir.toAbsolutePath.toString)

    //adjust the rpc port if one was specified
    GlobalData.rpcPortOpt = serverArgParser.rpcPortOpt match {
      case Some(rpcPort) => Some(rpcPort)
      case None          => GlobalData.rpcPortOpt //keep previous setting
    }

    implicit val appConfig: BitcoinSAppConfig =
      BitcoinSAppConfig.fromDatadirWithBundleConfWithServerArgs(
        datadirParser.datadir,
        serverArgParser)(system.dispatcher)

    val landingPane = new LandingPane(glassPane, serverArgParser)

    rootView.children = Vector(landingPane.view, glassPane)

    lazy val guiScene: Scene = new Scene(1400, 600) {
      root = rootView
      stylesheets = GlobalData.currentStyleSheets
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Bitcoin-S Wallet"
      scene = guiScene
      icons.add(GUIUtil.logo)
    }
    taskRunner

    ()
  }

  override lazy val glassPane: VBox = new VBox {
    children = new ProgressIndicator {
      progress = ProgressIndicator.IndeterminateProgress
      visible = true
    }
    alignment = Pos.Center
    visible = false
  }

  def changeToWalletGUIScene(): Unit = {
    Platform.runLater(
      rootView.children = Vector(
        borderPane,
        glassPane
      ))
  }

  override def stopApp(): Unit = {
    super.stopApp()
    sys.exit()
  }
}
