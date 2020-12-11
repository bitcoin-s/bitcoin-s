package org.bitcoins.gui.util

import scalafx.Includes._
import scalafx.scene.control.TextField

object GUIUtil {

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
