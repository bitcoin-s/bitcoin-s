package org.bitcoins.gui.util

import javafx.beans.value.ObservableValue
import org.bitcoins.core.protocol.BlockTimeStamp
import scalafx.scene.control.TextField

import java.text.NumberFormat
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Instant, ZoneOffset}
import scala.util.matching.Regex

object GUIUtil {

  val numericRegex: Regex = "-?([1-9,][0-9,]*)?".r
  val numberFormatter: NumberFormat = java.text.NumberFormat.getIntegerInstance

  def setNumericInput(textField: TextField): Unit = {
    textField.text.addListener {
      (_: ObservableValue[_ <: String], _: String, newVal: String) =>
        if (!newVal.matches(numericRegex.regex))
          textField.setText(newVal.replaceAll(numericRegex.regex, ""))
    }
  }

  def epochToDateString(epoch: BlockTimeStamp): String = {
    val long = epoch.toUInt32.toLong
    val instant = Instant.ofEpochSecond(long).atOffset(ZoneOffset.UTC)
    DateTimeFormatter
      .ofLocalizedDate(FormatStyle.MEDIUM)
      .format(instant)
  }
}
