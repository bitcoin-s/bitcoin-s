package org.bitcoins.gui.util

import javafx.beans.value.ObservableValue
import org.bitcoins.commons.jsonmodels.ExplorerEnv
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.core.protocol.tlv.{
  ContractInfoTLV,
  ContractInfoV0TLV,
  ContractInfoV1TLV,
  OracleAnnouncementTLV
}
import org.bitcoins.crypto.SchnorrPublicKey
import org.bitcoins.gui.{GUI, GlobalData}
import scalafx.beans.property.StringProperty
import scalafx.scene.control.{Button, TextField, Tooltip}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.layout.{Priority, Region}
import scalafx.scene.{Parent, Scene}
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Stage}

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
    val instant = Instant.ofEpochSecond(long)
    epochToDateString(instant)
  }

  def epochToDateString(instant: Instant): String = {
    val utc = instant.atOffset(ZoneOffset.UTC)
    DateTimeFormatter
      .ofLocalizedDate(FormatStyle.MEDIUM)
      .format(utc)
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

  def getFileChooserButton(handleFile: File => Unit): Button = new Button(
    "Browse...") {
    onAction = _ => {
      val _ = GUIUtil.showOpenDialog(handleFile)
    }
  }

  def getFileSaveButton(
      filename: String,
      bytes: Option[String],
      handleFile: Option[File => Unit]): Button = new Button("Browse...") {
    onAction = _ => {
      val _ = GUIUtil.showSaveDialog(filename, bytes, handleFile)
    }
  }

  def getCopyToClipboardButton(property: StringProperty): Button = new Button {
    styleClass ++= Vector("icon-button", "copy-button")
    disable <== property.isEmpty
    onAction = _ => {
      setStringToClipboard(property.value)
    }
    tooltip = Tooltip("Copy to Clipboard.")
    tooltip.value.setShowDelay(new javafx.util.Duration(100))
  }

  def getGreenCheck(): ImageView = {
    val img = new Image("/icons/green-check.png")
    val imageView = new ImageView(img)
    imageView.fitHeight = 16
    imageView.fitWidth = 16
    imageView
  }

  def getRedX(): ImageView = {
    val img = new Image("/icons/red-x.png")
    val imageView = new ImageView(img)
    imageView.fitHeight = 16
    imageView.fitWidth = 16
    imageView
  }

  def getHSpacer(): Region = new Region { hgrow = Priority.Always }

  def getVSpacer(): Region = new Region { vgrow = Priority.Always }

  def getWindow(
      windowTitle: String,
      width: Double,
      height: Double,
      rootView: Parent): Stage = {
    val windowScene = new Scene(width, height) {
      root = rootView
      stylesheets = GlobalData.currentStyleSheets
    }
    val stage = new Stage() {
      title = windowTitle
      scene = windowScene
      // Icon?
    }
    if (Properties.isMac || Properties.isLinux) {
      windowScene.accelerators.put(
        new KeyCodeCombination(KeyCode.W, KeyCombination.ShortcutDown),
        () => stage.close())
    }
    if (Properties.isWin || Properties.isLinux) {
      windowScene.accelerators.put(
        new KeyCodeCombination(KeyCode.F4, KeyCombination.AltDown),
        () => stage.close())
    }
    stage
  }

  def getAnnouncementUrl(
      network: BitcoinNetwork,
      primaryOracle: OracleAnnouncementTLV): String = {
    val baseUrl =
      ExplorerEnv.fromBitcoinNetwork(network).siteUrl
    s"${baseUrl}announcement/${primaryOracle.sha256.hex}"
  }

  def openUrl(url: String): Unit = {
    GUI.hostServices.showDocument(url)
  }

  val logo = new Image("/icons/bitcoin-s.png")
  val logoTestnet = new Image("/icons/bitcoin-s-testnet.png")
  val logoSignet = new Image("/icons/bitcoin-s-signet.png")
  val logoRegtest = new Image("/icons/bitcoin-s-regtest.png")

  def getOraclePubKeyEventId(
      contractInfo: ContractInfoTLV): (SchnorrPublicKey, String) = {
    contractInfo match {
      case ContractInfoV0TLV(_, _, oracleInfo) =>
        (oracleInfo.announcements.head.publicKey,
         oracleInfo.announcements.head.eventTLV.eventId)
      case ContractInfoV1TLV(_, contractOraclePairs) =>
        (contractOraclePairs.head._2.announcements.head.publicKey,
         contractOraclePairs.head._2.announcements.head.eventTLV.eventId)
    }
  }
}
