package org.bitcoins.gui.dlc

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.dlc.DLCStatus
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.Transaction
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
import scala.util.{Failure, Properties, Success, Try}

class DLCPaneModel(resultArea: TextArea, oracleInfoArea: TextArea) {
  var taskRunner: TaskRunner = _

  lazy val txPrintFunc: String => String = str => {
    // See if it was an error or not
    Try(Transaction.fromHex(str)) match {
      case Failure(_) =>
        // if it was print the error
        str
      case Success(tx) =>
        s"""|TxId: ${tx.txIdBE.hex}
            |
            |url: ${GlobalData.buildTxUrl(tx.txIdBE)}
            |
            |If the tx doesn't show up after a few minutes at this url you may need to manually
            |broadcast the tx with the full hex below
            |
            |Link to broadcast: ${GlobalData.broadcastUrl}
            |
            |Transaction: ${tx.hex}
      """.stripMargin
    }
  }

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

  def onInitContract(isEnum: Boolean): Unit = {
    val result = if (isEnum) {
      InitEnumContractDialog.showAndWait(parentWindow.value)
    } else {
      InitNumericContractDialog.showAndWait(parentWindow.value)
    }

    result match {
      case Some(contractDescriptor) =>
        val builder = new StringBuilder()

        val privKey = ECPrivateKey.freshPrivateKey
        val (kValues, oracleInfo) = contractDescriptor match {
          case EnumContractDescriptor(events) =>
            val kValue = ECPrivateKey.freshPrivateKey
            val rValue = kValue.schnorrNonce
            val oracleInfo = EnumSingleOracleInfo(
              OracleAnnouncementV0TLV
                .dummyForEventsAndKeys(privKey, rValue, events.map(_._1)))

            (Vector(kValue), oracleInfo)
          case (_, NumericContractDescriptor(_, numDigits, _)) =>
            val kValues =
              0.until(numDigits).map(_ => ECPrivateKey.freshPrivateKey).toVector
            val rValues = kValues.map(_.schnorrNonce)
            val oracleInfo = NumericSingleOracleInfo(
              OracleAnnouncementV0TLV.dummyForKeys(privKey, rValues))

            (kValues, oracleInfo)
        }

        val contractInfo = contractDescriptor match {
          case descriptor: EnumContractDescriptor =>
            ContractInfo(descriptor, oracleInfo.asInstanceOf[EnumOracleInfo])
          case (totalCollateral: Satoshis,
                descriptor: NumericContractDescriptor) =>
            ContractInfo(totalCollateral, descriptor, oracleInfo)
        }

        builder.append(s"Serialized Contract Info:\n${contractInfo.hex}\n\n")

        if (GlobalData.network != MainNet) {

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
                builder.append(s"$outcome - ${sig.hex}\n")
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

              val maxSigsStr = sigsMax.map(_.hex).mkString("\n")
              builder.append(s"local win sigs - $maxSigsStr\n\n\n")

              val middleSigsStr = sigsMiddle.map(_.hex).mkString("\n")
              builder.append(s"tie sigs - $middleSigsStr\n\n\n")

              val minSigsStr = sigsMin.map(_.hex).mkString("\n")
              builder.append(s"remote win sigs - $minSigsStr")
          }

          GlobalDLCData.lastOracleAnnouncement = oracleInfo.announcement.hex
        }

        GlobalDLCData.lastContractInfo = contractInfo.hex

        oracleInfoArea.text = builder.result()
      case None => ()
    }
  }

  def onOffer(): Unit = {
    printDLCDialogResult("CreateDLCOffer", new OfferDLCDialog)
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
    printDLCDialogResult("GetDLCFundingTx",
                         new GetFundingDLCDialog,
                         txPrintFunc)
  }

  def onExecute(): Unit = {
    printDLCDialogResult("ExecuteDLC", new ExecuteDLCDialog, txPrintFunc)
  }

  def onRefund(): Unit = {
    printDLCDialogResult("ExecuteDLCRefund", new RefundDLCDialog, txPrintFunc)
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
