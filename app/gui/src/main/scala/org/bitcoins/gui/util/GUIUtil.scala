package org.bitcoins.gui.util

import scalafx.scene.control.{TextField, TextFormatter}

import java.text.NumberFormat
import scala.util.control.NonFatal
import scala.util.matching.Regex

object GUIUtil {

  val numericRegex: Regex = "-?([1-9,][0-9,]*)?".r
  val numberFormatter: NumberFormat = java.text.NumberFormat.getIntegerInstance

  def setNumericInput(textField: TextField): Unit = {
    textField.textFormatter =
      new TextFormatter[String]((change: TextFormatter.Change) => {
        if (change.isContentChange) {
          val newText = change.getControlNewText
          if (
            newText.isEmpty || (!numericRegex.pattern.matcher(newText).matches)
          ) {
            change
          } else {
            val formatted = {
              try {
                val num = numberFormatter.parse(newText)
                numberFormatter.format(num)
              } catch {
                case NonFatal(_) => newText // allow input if error
              }
            }

            // replace with modified text
            change.setRange(0, change.getRangeEnd)
            change.setText(formatted)
            change.setCaretPosition(formatted.length)
            change.setAnchor(formatted.length)
            change
          }
        } else {
          change // no need for modification, if only the selection changes
        }
      })
  }
}
