package org.bitcoins.gui.settings

import org.bitcoins.gui.WalletGUI.stage

sealed abstract class Theme {
  def fileLocation: String

  def applyTheme: Boolean =
    stage.scene.value.getStylesheets.add(fileLocation)

  def undoTheme: Boolean =
    stage.scene.value.getStylesheets.removeAll(fileLocation)
}

object Themes {

  final case object DarkTheme extends Theme {
    override def fileLocation: String = "/themes/dark-theme.css"
  }

  val all: Vector[Theme] = Vector(DarkTheme)

  def fromString(str: String): Option[Theme] = {
    all.find(theme => str.toLowerCase() == theme.toString.toLowerCase)
  }
}
