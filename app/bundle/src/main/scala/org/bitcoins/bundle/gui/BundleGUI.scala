package org.bitcoins.bundle.gui

import com.typesafe.config.Config
import org.bitcoins.bundle.util.BitcoinSAppJFX3
import org.bitcoins.db.AppConfig
import org.bitcoins.db.AppConfig.DEFAULT_BITCOIN_S_DATADIR
import org.bitcoins.db.util.DatadirUtil
import org.bitcoins.gui._
import org.bitcoins.gui.util.GUIUtil
import scalafx.application.{JFXApp3, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.VBox

import java.nio.file.{Path, Paths}

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

    // Set log location
    val baseConfig: Config = AppConfig
      .getBaseConfig(DEFAULT_BITCOIN_S_DATADIR)
      .resolve()

    val datadir: Path =
      Paths.get(baseConfig.getString("bitcoin-s.datadir"))

    val usedDir = DatadirUtil.getFinalDatadir(datadir, baseConfig, None)

    System.setProperty("bitcoins.log.location", usedDir.toAbsolutePath.toString)

    val landingPane = new LandingPane(glassPane, commandLineArgs.toVector)
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
