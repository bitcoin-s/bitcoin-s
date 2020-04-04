package org.bitcoins.gui

import scalafx.beans.property.{DoubleProperty, StringProperty}

object GlobalData {
  val currentBalance: DoubleProperty = DoubleProperty(0)

  val log: StringProperty = StringProperty("")

  val statusText: StringProperty = StringProperty("")
}
