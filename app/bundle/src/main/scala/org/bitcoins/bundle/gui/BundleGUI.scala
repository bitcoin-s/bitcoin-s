package org.bitcoins.bundle.gui

import org.bitcoins.bundle.util.BitcoinSAppJFX3
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.gui._
import org.bitcoins.gui.util.GUIUtil
import org.bitcoins.server.BitcoinSAppConfig
import scalafx.application.{JFXApp3, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.VBox
import scalafx.stage.Screen

import java.util.prefs.Preferences

object BundleGUI extends WalletGUI with BitcoinSAppJFX3 {

  override val customFinalDirOpt: Option[String] = None

  override val actorSystemName: String =
    s"bitcoin-s-gui-${System.currentTimeMillis()}"

  override lazy val commandLineArgs: Array[String] = parameters.raw.toArray

  // Application Preference Keys
  val WINDOW_HEIGHT = "windowH"
  val WINDOW_WIDTH = "windowW"
  val WINDOW_X = "windowX"
  val WINDOW_Y = "windowY"
  val NODE_NAME = "BundleGUI"
  val preferences = Preferences.userRoot().node(NODE_NAME)

  val DEFAULT_WIDTH = 1400.0
  val DEFAULT_HEIGHT = 800.0
  val NOT_SET = Double.MinValue
  val MINIMUM_WIDTH = 800.0
  val MINIMUM_HEIGHT = 500.0

  var stageWidth =
    preferences.getDouble(WINDOW_WIDTH, DEFAULT_WIDTH)

  var stageHeight =
    preferences.getDouble(WINDOW_HEIGHT, DEFAULT_HEIGHT)

  var stageX = getPreferenceDoubleOpt(WINDOW_X, NOT_SET)
  var stageY = getPreferenceDoubleOpt(WINDOW_Y, NOT_SET)

  def getPreferenceDoubleOpt(pref: String, default: Double): Option[Double] = {
    val d = preferences.getDouble(pref, default)
    if (d != default) Some(d) else None
  }

  def validatePreferenceValues(): Unit = {
    // If the screen rectangle is not on some Screens, wipe saved x,y and use default position
    (stageX, stageY) match {
      case (Some(x), Some(y)) =>
        val screens = Screen.screensForRectangle(x, y, stageWidth, stageHeight)
        if (screens.isEmpty) {
          stageX = None
          stageY = None
        }
      case _ => // Not set, no validation
    }
  }

  def writePreferenceValues(): Unit = {
    preferences.putDouble(WINDOW_WIDTH, stage.getWidth)
    preferences.putDouble(WINDOW_HEIGHT, stage.getHeight)
    preferences.putDouble(WINDOW_X, stage.getX)
    preferences.putDouble(WINDOW_Y, stage.getY)
    preferences.flush()
  }

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
        serverArgParser)(system)

    if (appConfig.rpcPassword.nonEmpty) {
      GlobalData.setPassword(appConfig.rpcPassword)
    }

    validatePreferenceValues()

    val landingPane = new LandingPane(glassPane, serverArgParser)

    rootView.children = Vector(landingPane.view, glassPane)

    lazy val guiScene: Scene = new Scene() {
      root = rootView
      stylesheets = GlobalData.currentStyleSheets
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Bitcoin-S Wallet"
      scene = guiScene
      icons.add(GUIUtil.logo)
      minWidth = MINIMUM_WIDTH
      minHeight = MINIMUM_HEIGHT
    }
    positionStage()

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

  def applyStagePreferences(): Unit = {
    stage.setWidth(stageWidth)
    stage.setHeight(stageHeight)
    positionStage()
  }

  def positionStage(): Unit = {
    (stageX, stageY) match {
      case (Some(x), Some(y)) =>
        stage.setX(x)
        stage.setY(y)
      case _ =>
        stage.centerOnScreen()
    }
  }

  var walletLoaded = false

  def changeToWalletGUIScene(): Unit = {
    Platform.runLater(() => {
      rootView.children = Vector(
        borderPane,
        glassPane
      )
      applyStagePreferences()
      walletLoaded = true
    })
  }

  override def stopApp(): Unit = {
    super.stopApp()
    // Only save window position/size preferences if wallet was loaded
    if (walletLoaded) writePreferenceValues()
    sys.exit()
  }
}
