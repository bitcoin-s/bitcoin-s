package org.bitcoins.gui.util

import scalafx.Includes._
import scalafx.scene.control.TextField

import java.text.NumberFormat
import scala.util.matching.Regex

object GUIUtil {

  val numericRegex: Regex = "-?([1-9,][0-9,]*)?".r
  val numberFormatter: NumberFormat = java.text.NumberFormat.getIntegerInstance

  def setNumericInput(textField: TextField): Unit = {
    textField.text.addListener {
      (
          _: javafx.beans.value.ObservableValue[_ <: String],
          _: String,
          newVal: String) =>
        if (!newVal.matches("-?\\d*"))
          textField.setText(newVal.replaceAll("[^-?\\d]", ""))
    }
  }
}
