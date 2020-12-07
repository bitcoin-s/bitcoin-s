package org.bitcoins.gui

import org.bitcoins.cli.Config
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.gui.settings.Themes
import scalafx.beans.property.{DoubleProperty, StringProperty}

object GlobalData {
  val currentBalance: DoubleProperty = DoubleProperty(0)

  var network: BitcoinNetwork = _

  val log: StringProperty = StringProperty("")

  val statusText: StringProperty = StringProperty("")

  val darkThemeEnabled: Boolean = true

  def currentStyleSheets: Seq[String] =
    if (GlobalData.darkThemeEnabled) {
      Seq(Themes.DarkTheme.fileLocation)
    } else {
      Seq.empty
    }

  var rpcPortOpt: Option[Int] = None

  var debug = false

  def consoleCliConfig: Config =
    rpcPortOpt match {
      case None =>
        Config(debug = debug)
      case Some(rpcPort) =>
        Config(debug = debug, rpcPort = rpcPort)
    }
}
