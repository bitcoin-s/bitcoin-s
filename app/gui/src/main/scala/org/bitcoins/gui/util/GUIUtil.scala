package org.bitcoins.gui.util

import javafx.beans.value.ObservableValue
import org.bitcoins.core.protocol.BlockTimeStamp
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.image.Image
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import java.awt.Toolkit.getDefaultToolkit
import java.awt.datatransfer.StringSelection
import java.io.File
import java.nio.file.Files
import java.text.NumberFormat
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Instant, ZoneOffset}
import scala.util.Properties
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

  def setStringToClipboard(str: String): Unit = {
    val clipboard = getDefaultToolkit.getSystemClipboard
    val sel = new StringSelection(str)
    clipboard.setContents(sel, sel)
  }

  // fileChooser persists so initialDirectory can update across interactions
  private lazy val fileChooser = new FileChooser() {
    extensionFilters.addAll(txtExtensionFilter, allExtensionFilter)
    selectedExtensionFilter = txtExtensionFilter
    initialDirectory = new File(Properties.userHome)
  }

  private lazy val txtExtensionFilter =
    new ExtensionFilter("Text Files", "*.txt")
  private lazy val allExtensionFilter = new ExtensionFilter("All Files", "*")

  def showSaveDialog(
      bytes: String = null,
      filename: String = "",
      handleFile: File => Unit = null): Unit = {
    if (filename.nonEmpty) fileChooser.initialFileName = filename
    val chosenFileOpt = Option(fileChooser.showSaveDialog(null))
    chosenFileOpt match {
      case Some(chosenFile) =>
        if (bytes != null) {
          val _ = Files.write(chosenFile.toPath, bytes.getBytes)
        }
        // Remember last-used directory
        fileChooser.initialDirectory = chosenFile.getParentFile

        if (handleFile != null) {
          handleFile(chosenFile)
        }
      case None => // User canceled in dialog
    }
  }

  def showOpenDialog(handleFile: File => Unit = null): Option[File] = {
    val chosenFileOpt = Option(fileChooser.showOpenDialog(null))
    chosenFileOpt match {
      case Some(chosenFile) =>
        // Remember last-used directory
        fileChooser.initialDirectory = chosenFile.getParentFile

        if (handleFile != null) {
          handleFile(chosenFile)
        }
      case None => // User canceled in dialog
    }
    chosenFileOpt
  }

  def getFileChooserButton(handleFile: File => Unit = null) = {
    new Button("Browse...") {
      onAction = _ => {
        val _ = GUIUtil.showOpenDialog(handleFile)
      }
    }
  }

  def getFileSaveButton(
      bytes: String = null,
      filename: String = "",
      handleFile: File => Unit = null) = {
    new Button("Browse...") {
      onAction = _ => {
        val _ = GUIUtil.showSaveDialog(bytes, filename, handleFile)
      }
    }
  }

  val logo = new Image("/icons/bitcoin-s.png")
  val logoTestnet = new Image("/icons/bitcoin-s-testnet.png")
  val logoSignet = new Image("/icons/bitcoin-s-signet.png")
  val logoRegtest = new Image("/icons/bitcoin-s-regtest.png")
}
