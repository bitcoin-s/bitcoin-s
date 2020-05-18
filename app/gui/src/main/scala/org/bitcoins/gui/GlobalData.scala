package org.bitcoins.gui

import org.bitcoins.gui.settings.Themes
import scalafx.beans.property.{DoubleProperty, StringProperty}

object GlobalData {
  val currentBalance: DoubleProperty = DoubleProperty(0)

  val log: StringProperty = StringProperty("")

  val statusText: StringProperty = StringProperty("")

  val darkThemeEnabled: Boolean = true

  def currentStyleSheets: Seq[String] =
    if (GlobalData.darkThemeEnabled) {
      Seq(Themes.DarkTheme.fileLocation)
    } else {
      Seq.empty
    }

}
