package org.bitcoins.bundle.gui

import org.bitcoins.gui._
import org.bitcoins.gui.util.GUIUtil
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.VBox

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

  lazy val args = parameters.raw

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
