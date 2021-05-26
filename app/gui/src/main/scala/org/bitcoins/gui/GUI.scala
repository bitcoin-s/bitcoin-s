package org.bitcoins.gui

import org.bitcoins.cli.CliCommand.GetInfo
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.core.config._
import org.bitcoins.gui.util.GUIUtil._
import scalafx.application.JFXApp
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.image.Image
import scalafx.scene.layout.VBox

import scala.util._

object GUI extends WalletGUI with JFXApp {

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

  lazy val argsWithIndex: Vector[(String, Int)] =
    parameters.raw.zipWithIndex.toVector

  lazy val rpcPortOpt: Option[Int] = {
    lazy val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
    portOpt.map { case (_, idx) =>
      parameters.raw(idx + 1).toInt
    }
  }

  GlobalData.rpcPortOpt = rpcPortOpt

  lazy val debug: Boolean = {
    parameters.raw.exists(_.toLowerCase == "--debug")
  }

  GlobalData.debug = debug

  lazy val walletScene: Scene = new Scene(1400, 800) {
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

  lazy val info: BitcoinSServerInfo =
    ConsoleCli.exec(GetInfo, GlobalData.consoleCliConfig) match {
      case Failure(exception) =>
        throw exception
      case Success(str) =>
        val json = ujson.read(str)
        BitcoinSServerInfo.fromJson(json)
    }

  GlobalData.network = info.network

  lazy val (img, titleStr): (Image, String) = info.network match {
    case MainNet =>
      (logo, "Bitcoin-S Wallet")
    case TestNet3 =>
      (logoTestnet, "Bitcoin-S Wallet - [testnet]")
    case RegTest =>
      (logoRegtest, "Bitcoin-S Wallet - [regtest]")
    case SigNet =>
      (logoSignet, "Bitcoin-S Wallet - [signet]")
  }

  stage = new JFXApp.PrimaryStage {
    title = titleStr
    scene = walletScene
    icons.add(img)
  }

  fetchStartingData()
}
