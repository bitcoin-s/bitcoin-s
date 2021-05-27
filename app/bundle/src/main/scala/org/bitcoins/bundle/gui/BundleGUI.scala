package org.bitcoins.bundle.gui

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.core.config._
import org.bitcoins.db.AppConfig
import org.bitcoins.db.AppConfig.DEFAULT_BITCOIN_S_DATADIR
import org.bitcoins.gui._
import org.bitcoins.gui.util.GUIUtil
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.VBox

import java.nio.file.{Path, Paths}

object BundleGUI extends WalletGUI with JFXApp {

  // Catch unhandled exceptions on FX Application thread
  Thread
    .currentThread()
    .setUncaughtExceptionHandler((_: Thread, ex: Throwable) => {
      ex.printStackTrace()
      lazy val _ = new Alert(AlertType.Error) {
        initOwner(owner)
        title = "Unhandled exception"
        headerText = "Exception: " + ex.getClass + ""
        contentText = Option(ex.getMessage).getOrElse("")
      }.showAndWait()
    })

  implicit val system: ActorSystem = ActorSystem(
    s"bitcoin-s-gui-${System.currentTimeMillis()}")

  lazy val args = parameters.raw

  // Set log location
  {
    val baseConfig: Config = AppConfig
      .getBaseConfig(DEFAULT_BITCOIN_S_DATADIR)
      .resolve()

    val datadir: Path =
      Paths.get(baseConfig.getString("bitcoin-s.datadir"))

    val networkStr: String =
      baseConfig.getString("bitcoin-s.network")

    val network: BitcoinNetwork = networkStr.toLowerCase match {
      case "mainnet"  => MainNet
      case "main"     => MainNet
      case "testnet3" => TestNet3
      case "testnet"  => TestNet3
      case "test"     => TestNet3
      case "regtest"  => RegTest
      case "signet"   => SigNet
      case "sig"      => SigNet
      case _: String =>
        throw new IllegalArgumentException(s"Invalid network $networkStr")
    }

    val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }

    val usedDir = datadir.resolve(lastDirname)

    System.setProperty("bitcoins.log.location", usedDir.toAbsolutePath.toString)
  }

  val landingPane = new LandingPane(glassPane)
  rootView.children = Vector(landingPane.view, glassPane)

  lazy val guiScene: Scene = new Scene(1400, 600) {
    root = rootView
    stylesheets = GlobalData.currentStyleSheets
  }

  lazy val glassPane: VBox = new VBox {
    children = new ProgressIndicator {
      progress = ProgressIndicator.IndeterminateProgress
      visible = true
    }
    alignment = Pos.Center
    visible = false
  }

  stage = new JFXApp.PrimaryStage {
    title = "Bitcoin-S Wallet"
    scene = guiScene
    icons.add(GUIUtil.logo)
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
