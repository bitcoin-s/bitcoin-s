package org.bitcoins.gui.dlc

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.SerializedDLCStatus
import org.bitcoins.core.protocol.tlv.UnsignedNumericOutcome
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, Sha256DigestBE}
import org.bitcoins.gui.dlc.dialog._
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TextArea
import scalafx.stage.Window

import scala.util.{Failure, Success}

class DLCPaneModel(resultArea: TextArea, oracleInfoArea: TextArea) {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  val dlcs: ObservableBuffer[SerializedDLCStatus] =
    new ObservableBuffer[SerializedDLCStatus]()

  def getDLCs: Vector[SerializedDLCStatus] = {
    ConsoleCli.exec(GetDLCs, Config.empty) match {
      case Failure(exception) => throw exception
      case Success(dlcsStr) =>
        ujson.read(dlcsStr).arr.map(SerializedDLCStatus.fromJson).toVector
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
        dlcs += SerializedDLCStatus.fromJson(ujson.read(dlcStatus))
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
      dialog: DLCDialog[T]): Unit = {
    val result = dialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) => resultArea.text = commandReturn
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
      case Some(contractInfo) =>
        val builder = new StringBuilder()

        val privKey = ECPrivateKey.freshPrivateKey
        val pubKey = privKey.schnorrPublicKey
        val (kValues, rValues, oracleInfo) = contractInfo match {
          case SingleNonceContractInfo(_) =>
            val kValue = ECPrivateKey.freshPrivateKey
            val rValue = kValue.schnorrNonce
            val oracleInfo = SingleNonceOracleInfo(pubKey, rValue)

            (Vector(kValue), Vector(rValue), oracleInfo)
          case MultiNonceContractInfo(_, _, numDigits, _) =>
            val kValues =
              0.until(numDigits).map(_ => ECPrivateKey.freshPrivateKey).toVector
            val rValues = kValues.map(_.schnorrNonce)
            val oracleInfo = MultiNonceOracleInfo(pubKey, rValues)

            (kValues, rValues, oracleInfo)
        }

        builder.append(
          s"Oracle Public Key: ${pubKey.hex}\nEvent R values: ${rValues.map(_.hex).mkString(",")}\n")
        builder.append(s"Serialized Oracle Info: ${oracleInfo.hex}\n\n")

        builder.append(
          s"\nSerialized Contract Info:\n${contractInfo.toTLV.hex}\n\n")

        contractInfo match {
          case contractInfo: SingleNonceContractInfo =>
            builder.append("Outcomes and oracle sigs in order of entry:\n")
            contractInfo.keys.foreach { outcome =>
              val bytes = outcome.serialized.head
              val hash = CryptoUtil.sha256(bytes).bytes
              val sig = privKey.schnorrSignWithNonce(hash, kValues.head)
              builder.append(s"$outcome - ${sig.hex}\n")
            }
          case contractInfo: MultiNonceContractInfo =>
            builder.append("Oracle sigs:\n")

            val sortedOutcomes = contractInfo.outcomeVec.sortBy(_._2)

            val max = UnsignedNumericOutcome(sortedOutcomes.last._1)
            val middle = UnsignedNumericOutcome(
              sortedOutcomes(sortedOutcomes.size / 2)._1)
            val min = UnsignedNumericOutcome(sortedOutcomes.head._1)

            val sigsMax =
              max.serialized.zip(kValues.take(max.digits.size)).map {
                case (bytes, kValue) =>
                  val hash = CryptoUtil.sha256(bytes).bytes
                  privKey.schnorrSignWithNonce(hash, kValue)
              }

            val sigsMiddle =
              middle.serialized.zip(kValues.take(middle.digits.size)).map {
                case (bytes, kValue) =>
                  val hash = CryptoUtil.sha256(bytes).bytes
                  privKey.schnorrSignWithNonce(hash, kValue)
              }

            val sigsMin =
              min.serialized.zip(kValues.take(min.digits.size)).map {
                case (bytes, kValue) =>
                  val hash = CryptoUtil.sha256(bytes).bytes
                  privKey.schnorrSignWithNonce(hash, kValue)
              }

            val maxSigsStr = sigsMax.map(_.hex).mkString("\n")
            builder.append(s"local win sigs - $maxSigsStr\n\n\n")

            val middleSigsStr = sigsMiddle.map(_.hex).mkString("\n")
            builder.append(s"tie sigs - $middleSigsStr\n\n\n")

            val minSigsStr = sigsMin.map(_.hex).mkString("\n")
            builder.append(s"remote win sigs - $minSigsStr")
        }

        GlobalDLCData.lastOracleInfo = oracleInfo.hex
        GlobalDLCData.lastContractInfo = contractInfo.toTLV.hex

        oracleInfoArea.text = builder.result()
      case None => ()
    }
  }

  def onOffer(): Unit = {
    printDLCDialogResult("CreateDLCOffer", OfferDLCDialog)
  }

  def onAccept(): Unit = {
    printDLCDialogResult("AcceptDLCOffer", AcceptDLCDialog)
  }

  def onSign(): Unit = {
    printDLCDialogResult("SignDLC", SignDLCDialog)
  }

  def onAddSigs(): Unit = {
    printDLCDialogResult("AddDLCSigs", AddSigsDLCDialog)
  }

  def onGetFunding(): Unit = {
    printDLCDialogResult("GetDLCFundingTx", GetFundingDLCDialog)
  }

  def onClose(): Unit = {
    printDLCDialogResult("ExecuteDLC", ExecuteDLCDialog)
  }

  def onRefund(): Unit = {
    printDLCDialogResult("ExecuteDLCRefund", RefundDLCDialog)
  }

  def viewDLC(status: SerializedDLCStatus): Unit = {
    updateDLCs()
    val updatedStatus = dlcs.find(_.tempContractId == status.tempContractId)
    ViewDLCDialog.showAndWait(parentWindow.value,
                              updatedStatus.getOrElse(status))
  }
}
