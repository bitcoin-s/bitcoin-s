package org.bitcoins.gui.dlc

import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, Sha256DigestBE}
import org.bitcoins.gui.dlc.dialog._
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TextArea
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Window}
import upickle.default._

import java.io.File
import java.nio.file.Files
import scala.util.{Failure, Properties, Success}

class DLCPaneModel(resultArea: TextArea, oracleInfoArea: TextArea)
    extends Logging {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  val dlcs: ObservableBuffer[DLCStatus] =
    new ObservableBuffer[DLCStatus]()

  def getDLCs: Vector[DLCStatus] = {
    ConsoleCli.exec(GetDLCs, Config.empty) match {
      case Failure(exception) => throw exception
      case Success(dlcsStr) =>
        ujson.read(dlcsStr).arr.map(read[DLCStatus]).toVector
    }
  }

  def setUp(): Unit = {
    dlcs.clear()
    dlcs ++= getDLCs
  }

  def updateDLC(paramHash: Sha256DigestBE): Unit = {
    ConsoleCli.exec(GetDLC(paramHash), Config.empty) match {
      case Failure(exception) => throw exception
      case Success(dlcStatus) =>
        dlcs += read[DLCStatus](ujson.read(dlcStatus))
        dlcs.find(_.paramHash == paramHash).foreach(dlcs -= _)
    }
  }

  def updateDLCs(): Unit = {
    val newDLCs = getDLCs
    val toAdd = newDLCs.diff(dlcs)
    val toRemove = dlcs.diff(newDLCs)
    dlcs ++= toAdd
    dlcs --= toRemove
  }

  def printDLCDialogResult[T <: CliCommand](
      caption: String,
      dialog: DLCDialog[T],
      postProcessStr: String => String = str => str): Unit = {
    val result = dialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                resultArea.text = postProcessStr(commandReturn)
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  private def printDummyOracleInfo(
      builder: StringBuilder,
      privKey: ECPrivateKey,
      oracleInfo: SingleOracleInfo,
      kValues: Vector[ECPrivateKey],
      contractInfo: ContractInfo): StringBuilder = {
    if (GlobalData.network != MainNet && kValues.nonEmpty) {
      val eventId = oracleInfo.announcement.eventTLV match {
        case v0: OracleEventV0TLV => v0.eventId
      }

      builder.append(
        s"Oracle Public Key: ${oracleInfo.publicKey.hex}\nEvent R values: ${oracleInfo.nonces.map(_.hex).mkString(",")}\n\n")

      builder.append(
        s"Serialized Oracle Announcement: ${oracleInfo.announcement.hex}\n\n")

      contractInfo.contractDescriptor match {
        case descriptor: EnumContractDescriptor =>
          builder.append("Outcomes and oracle sigs in order of entry:\n")
          descriptor.keys.foreach { outcome =>
            val bytes = outcome.serialized.head
            val hash = CryptoUtil
              .sha256DLCAttestation(bytes)
              .bytes
            val sig = privKey.schnorrSignWithNonce(hash, kValues.head)
            val sigTlv =
              OracleAttestmentV0TLV(eventId,
                                    privKey.schnorrPublicKey,
                                    Vector(sig),
                                    Vector(outcome.outcome))

            builder.append(s"$outcome - ${sigTlv.hex}\n")
          }
        case _: NumericContractDescriptor =>
          builder.append("Oracle sigs:\n")

          val sortedOutcomes =
            contractInfo.allOutcomesAndPayouts.sortBy(_._2)

          val max = sortedOutcomes.last._1.outcome
            .asInstanceOf[UnsignedNumericOutcome]
          val middle = sortedOutcomes(sortedOutcomes.size / 2)._1.outcome
            .asInstanceOf[UnsignedNumericOutcome]
          val min = sortedOutcomes.head._1.outcome
            .asInstanceOf[UnsignedNumericOutcome]

          val sigsMax =
            max.serialized.zip(kValues.take(max.digits.size)).map {
              case (bytes, kValue) =>
                val hash = CryptoUtil
                  .sha256DLCAttestation(bytes)
                  .bytes
                privKey.schnorrSignWithNonce(hash, kValue)
            }

          val sigsMiddle =
            middle.serialized.zip(kValues.take(middle.digits.size)).map {
              case (bytes, kValue) =>
                val hash = CryptoUtil
                  .sha256DLCAttestation(bytes)
                  .bytes
                privKey.schnorrSignWithNonce(hash, kValue)
            }

          val sigsMin =
            min.serialized.zip(kValues.take(min.digits.size)).map {
              case (bytes, kValue) =>
                val hash = CryptoUtil
                  .sha256DLCAttestation(bytes)
                  .bytes
                privKey.schnorrSignWithNonce(hash, kValue)
            }

          val maxSigsStr =
            OracleAttestmentV0TLV(eventId,
                                  privKey.schnorrPublicKey,
                                  sigsMax,
                                  max.digits.map(_.toString)).hex
          builder.append(s"local win sigs - $maxSigsStr\n\n\n")

          val middleSigsStr =
            OracleAttestmentV0TLV(eventId,
                                  privKey.schnorrPublicKey,
                                  sigsMiddle,
                                  middle.digits.map(_.toString)).hex
          builder.append(s"tie sigs - $middleSigsStr\n\n\n")

          val minSigsStr =
            OracleAttestmentV0TLV(eventId,
                                  privKey.schnorrPublicKey,
                                  sigsMin,
                                  min.digits.map(_.toString)).hex
          builder.append(s"remote win sigs - $minSigsStr")
      }

      GlobalDLCData.lastOracleAnnouncement = oracleInfo.announcement.hex
    }
    builder
  }

  def onInitEnumContractDialog(): Unit = {
    val result = InitEnumContractDialog.showAndWait(parentWindow.value)

    result match {
      case Some((contractDescriptor, announcementOpt)) =>
        val privKey = ECPrivateKey.freshPrivateKey

        val (kValues, oracleInfo) = announcementOpt match {
          case Some(announcement) =>
            (Vector.empty, EnumSingleOracleInfo(announcement))
          case None =>
            val kValue = ECPrivateKey.freshPrivateKey
            val rValue = kValue.schnorrNonce
            val oracleInfo = EnumSingleOracleInfo(
              OracleAnnouncementV0TLV
                .dummyForEventsAndKeys(privKey,
                                       rValue,
                                       contractDescriptor.map(_._1).toVector))

            (Vector(kValue), oracleInfo)
        }
        val builder = new StringBuilder()

        val contractInfo = ContractInfo(contractDescriptor, oracleInfo)
        builder.append(s"Serialized Contract Info:\n${contractInfo.hex}\n\n")
        GlobalDLCData.lastContractInfo = contractInfo.hex

        printDummyOracleInfo(builder,
                             privKey,
                             oracleInfo,
                             kValues,
                             contractInfo)

        oracleInfoArea.text = builder.result()
      case None => ()
    }
  }

  def onInitNumericContractDialog(): Unit = {
    val result = InitNumericContractDialog.showAndWait(parentWindow.value)

    result match {
      case Some((totalCol, contractDescriptor, announcementOpt)) =>
        val privKey = ECPrivateKey.freshPrivateKey

        val (kValues, oracleInfo) = announcementOpt match {
          case Some(announcement) =>
            (Vector.empty, EnumSingleOracleInfo(announcement))
          case None =>
            val kValues =
              0.until(contractDescriptor.numDigits)
                .map(_ => ECPrivateKey.freshPrivateKey)
                .toVector
            val rValues = kValues.map(_.schnorrNonce)
            val oracleInfo = NumericSingleOracleInfo(
              OracleAnnouncementV0TLV.dummyForKeys(privKey, rValues))

            (kValues, oracleInfo)
        }
        val builder = new StringBuilder()

        val pairOpt = ContractOraclePair.fromDescriptorOracleOpt(
          contractDescriptor,
          oracleInfo)
        pairOpt match {
          case Some(pair) =>
            val contractInfo =
              ContractInfo(totalCol, pair)
            builder.append(
              s"Serialized Contract Info:\n${contractInfo.hex}\n\n")
            GlobalDLCData.lastContractInfo = contractInfo.hex

            printDummyOracleInfo(builder,
                                 privKey,
                                 oracleInfo,
                                 kValues,
                                 contractInfo)

            oracleInfoArea.text = builder.result()
          case None =>
            //i think doing nothing is right here?
            logger.warn(
              s"Invalid contract/oracle pairing, contract=$contractDescriptor oracle=$oracleInfo")
            ()
        }

      case None => ()
    }
  }

  def onOffer(): Unit = {
    val result = InitEnumOfferDialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = "Create DLC Offer",
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) =>
                resultArea.text = commandReturn
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onAccept(): Unit = {
    printDLCDialogResult("AcceptDLCOffer", new AcceptDLCDialog)
  }

  def onSign(): Unit = {
    printDLCDialogResult("SignDLC", new SignDLCDialog)
  }

  def onAddSigs(): Unit = {
    printDLCDialogResult("AddDLCSigs", new AddSigsDLCDialog)
  }

  def onGetFunding(): Unit = {
    printDLCDialogResult("GetDLCFundingTx", new GetFundingDLCDialog)
  }

  def onExecute(): Unit = {
    printDLCDialogResult("ExecuteDLC", new ExecuteDLCDialog)
  }

  def onRefund(): Unit = {
    printDLCDialogResult("ExecuteDLCRefund", new RefundDLCDialog)
  }

  def viewDLC(status: DLCStatus): Unit = {
    updateDLCs()
    val updatedStatus = dlcs.find(_.tempContractId == status.tempContractId)
    ViewDLCDialog.showAndWait(parentWindow.value,
                              updatedStatus.getOrElse(status),
                              this)
  }

  def exportResult(result: String): Unit = {
    val txtFilter = new ExtensionFilter("Text Files", "*.txt")
    val allExtensionFilter = new ExtensionFilter("All Files", "*")
    val fileChooser = new FileChooser() {
      extensionFilters.addAll(txtFilter, allExtensionFilter)
      selectedExtensionFilter = txtFilter
      initialDirectory = new File(Properties.userHome)
    }

    val selectedFile = fileChooser.showSaveDialog(null)

    taskRunner.run(
      "Export Result",
      op = {
        if (selectedFile != null) {
          val bytes = result.getBytes

          Files.write(selectedFile.toPath, bytes)
          ()
        }
      }
    )
  }
}
