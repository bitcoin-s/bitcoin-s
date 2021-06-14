package org.bitcoins.gui.dlc.dialog

import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand._
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

object SignDLCDialog extends Logging {

  def showAndWait(parentWindow: Window): Option[SignDLCCliCommand] = {
    val dialog = new Dialog[Option[SignDLCCliCommand]]() {
      initOwner(parentWindow)
      title = "Sign DLC"
      headerText = "Enter DLC Accept message"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    var dlcDetailsShown = false
    val acceptTLVTF = new TextField() {
      minWidth = 300
    }

    val acceptTFHBox = new HBox() {
      spacing = 5
      children = Vector(new Label("DLC Accept"), acceptTLVTF)
    }

    val separatorHBox = new HBox() {
      alignment = Pos.Center
      alignmentInParent = Pos.Center
      spacing = 5
      minWidth <== acceptTFHBox.width
      children = Vector(new Separator(), new Label("or"), new Separator())
    }

    val errorLabel = new Label("") {
      style = "-fx-text-fill: red"
    }

    val fromFileHBox = new HBox() {
      spacing = 5
      children = Vector(new Label("DLC Accept File"))
    }

    val vbox = new VBox() {
      margin = Insets(10)
      spacing = 10
      children = Vector(acceptTFHBox, separatorHBox, fromFileHBox)
    }

    var nextRow: Int = 2
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5
    }

    val destinationChooser = DLCDialog.fileChooserButton(
      open = false,
      { file =>
        DLCDialog.signDestDLCFile = Some(file)
        DLCDialog.signDestFileChosenLabel.text = file.toString
      })

    val destChooserHBox = new HBox() {
      spacing = 5
      children = Vector(destinationChooser, DLCDialog.signDestFileChosenLabel)
    }

    def showDLCTerms(status: DLCStatus, isFromFile: Boolean): Unit = {
      vbox.children.clear()
      if (isFromFile) {
        vbox.children.add(fromFileHBox)
      } else {
        vbox.children.addAll(acceptTFHBox)
      }
      vbox.children.add(gridPane)

      val (oracleKey, eventId) =
        status.contractInfo.oracleInfos.head.toTLV match {
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

      status.contractInfo.contractDescriptors.head.toTLV match {
        case v0: ContractDescriptorV0TLV =>
          gridPane.add(new Label("Potential Outcome"), 0, nextRow)
          gridPane.add(new Label("Payouts"), 1, nextRow)
          nextRow += 1

          val descriptor = EnumContractDescriptor.fromTLV(v0)

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

      if (isFromFile) {
        gridPane.add(new Label(DLCDialog.dlcSignFileDestStr), 0, nextRow)
        gridPane.add(destChooserHBox, 1, nextRow)
        nextRow += 1
      }
    }

    acceptTLVTF.onKeyTyped = _ => {
      if (!dlcDetailsShown) {
        Try(
          LnMessageFactory(DLCAcceptTLV).fromHex(
            acceptTLVTF.text.value)) match {
          case Failure(_) => ()
          case Success(lnMessage) =>
            showDetails(lnMessage, isFromFile = false)
        }
      }
    }

    def showDetails(
        lnMessage: LnMessage[DLCAcceptTLV],
        isFromFile: Boolean): Unit = {
      val tempId = lnMessage.tlv.tempContractId
      dlcs.find(_.tempContractId == tempId) match {
        case Some(dlc) =>
          dlcDetailsShown = true
          showDLCTerms(dlc, isFromFile)
        case None =>
          val errMsg =
            s"DLCAccept is not associated with a DLC in our DLC Table View"
          logger.error(errMsg)
          errorLabel.text = errMsg
          vbox.children.add(errorLabel)
      }
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    def handleFileChosen(file: File): Unit = {
      val acceptMessageT = Try {
        val hex = Files.readAllLines(file.toPath).get(0)
        LnMessageFactory(DLCAcceptTLV).fromHex(hex)
      }

      acceptMessageT match {
        case Failure(_) =>
          val errMsg = "Error, file chosen as not a valid DLCAccept message"
          logger.error(errMsg)
          errorLabel.text = errMsg
          vbox.children.add(errorLabel)
          dialog.dialogPane().getScene.getWindow.sizeToScene()
        case Success(acceptMessage) =>
          showDetails(acceptMessage, isFromFile = true)
      }
      ()
    }

    val fileChooser = DLCDialog.fileChooserButton(
      open = true,
      { file =>
        DLCDialog.acceptDLCFile = Some(file)
        DLCDialog.acceptFileChosenLabel.text = file.toString
        handleFileChosen(file)
      })

    fromFileHBox.children.addAll(fileChooser, DLCDialog.acceptFileChosenLabel)

    dialog.dialogPane().content = new ScrollPane {
      margin = Insets(10)
      content = vbox
    }

    // When the OK button is clicked, convert the result to a SignDLC.
    dialog.resultConverter = dialogButton => {
      val res = if (dialogButton == ButtonType.OK) {
        DLCDialog.acceptDLCFile match {
          case Some(file) =>
            val destOpt = DLCDialog.signDestDLCFile.map(_.toPath)
            Some(SignDLCFromFile(file.toPath, destOpt))
          case None =>
            val acceptHex = acceptTLVTF.text.value
            val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptHex)

            Some(SignDLC(accept))
        }
      } else None

      // reset
      DLCDialog.acceptDLCFile = None
      DLCDialog.acceptFileChosenLabel.text = ""
      DLCDialog.signDestDLCFile = None
      DLCDialog.signDestFileChosenLabel.text = ""

      res
    }

    val result = dialog.showAndWait()

    result match {
      case Some(Some(cmd: SignDLCCliCommand)) =>
        Some(cmd)
      case Some(_) | None => None
    }
  }
}
