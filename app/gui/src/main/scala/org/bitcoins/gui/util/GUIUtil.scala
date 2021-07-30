package org.bitcoins.gui.util

import javafx.beans.value.ObservableValue
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.gui.GlobalData
import scalafx.scene.{Parent, Scene}
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.image.Image
import scalafx.scene.layout.{Priority, Region}
import scalafx.stage.{FileChooser, Stage}
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
      filename: String,
      bytesOpt: Option[String],
      handleFileOpt: Option[File => Unit]): Unit = {
    fileChooser.initialFileName = filename
    val chosenFileOpt = Option(fileChooser.showSaveDialog(null))
    chosenFileOpt match {
      case Some(chosenFile) =>
        // Remember last-used directory
        fileChooser.initialDirectory = chosenFile.getParentFile

        bytesOpt match {
          case Some(bytes) => Files.write(chosenFile.toPath, bytes.getBytes)
          case None        => // There was nothing sent in to write out
        }

        handleFileOpt match {
          case Some(handleFile) => handleFile(chosenFile)
          case None             => // No callback defined
        }
      case None => // User canceled in dialog
    }
  }

  def showOpenDialog(handleFile: File => Unit): Option[File] = {
    val chosenFileOpt = Option(fileChooser.showOpenDialog(null))
    chosenFileOpt match {
      case Some(chosenFile) =>
        // Remember last-used directory
        fileChooser.initialDirectory = chosenFile.getParentFile

        handleFile(chosenFile)
      case None => // User canceled in dialog
    }
    chosenFileOpt
  }

  def getFileChooserButton(handleFile: File => Unit) = {
    new Button("Browse...") {
      onAction = _ => {
        val _ = GUIUtil.showOpenDialog(handleFile)
      }
    }
  }

  def getFileSaveButton(
      filename: String,
      bytes: Option[String],
      handleFile: Option[File => Unit]) = {
    new Button("Browse...") {
      onAction = _ => {
        val _ = GUIUtil.showSaveDialog(filename, bytes, handleFile)
      }
    }
  }

  def getHSpacer() = {
    new Region { hgrow = Priority.Always }
  }

  def getWindow(
      windowTitle: String,
      width: Double,
      height: Double,
      rootView: Parent) = {
    val windowScene = new Scene(width, height) {
      root = rootView
      stylesheets = GlobalData.currentStyleSheets
    }
    val stage = new Stage() {
      title = windowTitle
      scene = windowScene
      // Icon?
    }
    stage
  }

  val logo = new Image("/icons/bitcoin-s.png")
  val logoTestnet = new Image("/icons/bitcoin-s-testnet.png")
  val logoSignet = new Image("/icons/bitcoin-s-signet.png")
  val logoRegtest = new Image("/icons/bitcoin-s-regtest.png")
}
