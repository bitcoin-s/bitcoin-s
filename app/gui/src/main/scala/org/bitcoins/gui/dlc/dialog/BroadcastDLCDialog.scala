package org.bitcoins.gui.dlc.dialog

import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.dlc.models.DLCStatus.getContractId
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.DLCPlotUtil
import org.bitcoins.gui.dlc.GlobalDLCData.dlcs
import org.bitcoins.gui.util.GUIUtil
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import java.io.File
import java.nio.file.Files
import scala.collection._
import scala.util.{Failure, Success, Try}

object BroadcastDLCDialog extends Logging {

  def showAndWait(
      parentWindow: Window): Option[AddDLCSigsAndBroadcastCliCommand] = {
    val dialog = new Dialog[Option[AddDLCSigsAndBroadcastCliCommand]]() {
      initOwner(parentWindow)
      title = "Add DLC Signatures"
      headerText = "Enter DLC signatures message"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    var dlcDetailsShown = false
    val signTLVTF = new TextField() {
      minWidth = 300
    }

    val signTFHBox = new HBox() {
      spacing = 5
      children = Vector(new Label("DLC Signatures"), signTLVTF)
    }

    val separatorHBox = new HBox() {
      alignment = Pos.Center
      alignmentInParent = Pos.Center
      spacing = 5
      minWidth <== signTFHBox.width
      children = Vector(new Separator(), new Label("or"), new Separator())
    }

    val errorLabel = new Label("") {
      style = "-fx-text-fill: red"
    }

    val fromFileHBox = new HBox() {
      spacing = 5
      children = Vector(new Label("DLC Sign File"))
    }

    val vbox = new VBox() {
      margin = Insets(10)
      spacing = 10
      children = Vector(signTFHBox, separatorHBox, fromFileHBox)
    }

    var nextRow: Int = 2
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(10)
      hgap = 5
      vgap = 5
    }

    def showDLCTerms(status: DLCStatus, isFromFile: Boolean): Unit = {
      vbox.children.clear()
      if (isFromFile) {
        vbox.children.add(fromFileHBox)
      } else {
        vbox.children.addAll(signTFHBox)
      }
      vbox.children.add(gridPane)

      val (oracleKey, eventId) = status.contractInfo.oracleInfo.toTLV match {
        case OracleInfoV0TLV(announcement) =>
          (announcement.publicKey.hex, announcement.eventTLV.eventId)
        case _: MultiOracleInfoTLV =>
          throw new RuntimeException("This is impossible.")
      }

      gridPane.add(new Label("Event Id"), 0, nextRow)
      gridPane.add(
        new TextField() {
          text = eventId
          editable = false
          minWidth = 300
        },
        1,
        nextRow
      )
      nextRow += 1

      gridPane.add(new Label("Oracle Public Key"), 0, nextRow)
      gridPane.add(
        new TextField() {
          text = oracleKey
          editable = false
        },
        1,
        nextRow
      )
      nextRow += 1

      gridPane.add(new Label("Your Collateral"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = status.localCollateral.satoshis.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(new Label("Counter Party Collateral"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = status.remoteCollateral.satoshis.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      status.contractInfo.contractDescriptor.toTLV match {
        case v0: ContractDescriptorV0TLV =>
          gridPane.add(new Label("Potential Outcome"), 0, nextRow)
          gridPane.add(new Label("Payouts"), 1, nextRow)
          nextRow += 1

          val descriptor = EnumContractDescriptor
            .fromTLV(v0)
            .flip(status.totalCollateral.satoshis)

          descriptor.foreach { case (str, satoshis) =>
            gridPane.add(new TextField() {
                           text = str.outcome
                           editable = false
                         },
                         0,
                         nextRow)
            gridPane.add(new TextField() {
                           text = satoshis.toString
                           editable = false
                         },
                         1,
                         nextRow)
            nextRow += 1
          }
        case v1: ContractDescriptorV1TLV =>
          val previewGraphButton: Button = new Button("Preview Graph") {
            onAction = _ => {
              val descriptor = NumericContractDescriptor.fromTLV(v1)
              val payoutCurve = if (status.isInitiator) {
                descriptor.outcomeValueFunc
              } else {
                descriptor
                  .flip(status.totalCollateral.satoshis)
                  .outcomeValueFunc
              }

              DLCPlotUtil.plotCETs(base = 2,
                                   descriptor.numDigits,
                                   payoutCurve,
                                   status.contractInfo.totalCollateral,
                                   descriptor.roundingIntervals,
                                   None)
              ()
            }
          }

          gridPane.add(new Label("Payout Function"), 0, nextRow)
          gridPane.add(previewGraphButton, 1, nextRow)
          nextRow += 1
      }

      gridPane.add(new Label("Fee Rate"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = status.feeRate.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(new Label("Refund Date"), 0, nextRow)
      gridPane.add(
        new TextField() {
          text = GUIUtil.epochToDateString(status.timeouts.contractTimeout)
          editable = false
        },
        1,
        nextRow)
      nextRow += 1
    }

    signTLVTF.onKeyTyped = _ => {
      if (!dlcDetailsShown) {
        Try(
          LnMessageFactory(DLCSignTLV).fromHex(
            signTLVTF.text.value.trim)) match {
          case Failure(_) => ()
          case Success(lnMessage) =>
            showDetails(lnMessage, isFromFile = false)
        }
      }
    }

    def showDetails(
        lnMessage: LnMessage[DLCSignTLV],
        isFromFile: Boolean): Unit = {
      val tempId = lnMessage.tlv.contractId
      dlcs.find(getContractId(_).contains(tempId)) match {
        case Some(dlc) =>
          dlcDetailsShown = true
          showDLCTerms(dlc, isFromFile)
        case None =>
          val errMsg =
            s"DLCSign is not associated with a DLC in our DLC Table View"
          logger.error(errMsg)
          errorLabel.text = errMsg
          vbox.children.add(errorLabel)
      }
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    def handleFileChosen(file: File): Unit = {
      val signMessageT = Try {
        val hex = Files.readAllLines(file.toPath).get(0)
        LnMessageFactory(DLCSignTLV).fromHex(hex)
      }

      signMessageT match {
        case Failure(_) =>
          val errMsg = "Error, file chosen as not a valid DLCSign message"
          logger.error(errMsg)
          errorLabel.text = errMsg
          vbox.children.add(errorLabel)
          dialog.dialogPane().getScene.getWindow.sizeToScene()
        case Success(sign) =>
          showDetails(sign, isFromFile = true)
      }
      ()
    }

    val fileChooser = DLCDialog.fileChooserButton(
      open = true,
      { file =>
        DLCDialog.signDLCFile = Some(file)
        DLCDialog.signFileChosenLabel.text = file.toString
        handleFileChosen(file)
      })

    fromFileHBox.children.addAll(fileChooser, DLCDialog.signFileChosenLabel)

    dialog.dialogPane().content = new ScrollPane {
      margin = Insets(10)
      content = vbox
    }

    // When the OK button is clicked, convert the result to a SignDLC.
    dialog.resultConverter = dialogButton => {
      val res = if (dialogButton == ButtonType.OK) {
        DLCDialog.signDLCFile match {
          case Some(file) =>
            Some(AddDLCSigsAndBroadcastFromFile(file.toPath))
          case None =>
            val hex = signTLVTF.text.value
            val signTLV = LnMessageFactory(DLCSignTLV)(hex)

            Some(AddDLCSigsAndBroadcast(signTLV))
        }
      } else None

      // reset
      DLCDialog.signDLCFile = None
      DLCDialog.signFileChosenLabel.text = ""

      res
    }

    val result = dialog.showAndWait()

    result match {
      case Some(Some(cmd: AddDLCSigsAndBroadcastCliCommand)) =>
        Some(cmd)
      case Some(_) | None => None
    }
  }
}
