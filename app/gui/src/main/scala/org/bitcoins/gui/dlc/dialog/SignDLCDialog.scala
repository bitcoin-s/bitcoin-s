package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.dlc.models.{DLCStatus, EnumContractDescriptor}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.gui.GlobalData
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

object SignDLCDialog {

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
      val descriptor = status.contractInfo.contractDescriptor.toTLV match {
        case v0: ContractDescriptorV0TLV =>
          EnumContractDescriptor
            .fromTLV(v0)
        case _: ContractDescriptorV1TLV =>
          throw new RuntimeException("This is impossible.")
      }

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

      gridPane.add(new Label("Potential Outcome"), 0, nextRow)
      gridPane.add(new Label("Payouts"), 1, nextRow)
      nextRow += 1

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
          dlc.contractInfo.contractDescriptor.toTLV match {
            case _: ContractDescriptorV0TLV =>
              dlcDetailsShown = true
              showDLCTerms(dlc, isFromFile)
              dialog.dialogPane().getScene.getWindow.sizeToScene()
            case _: ContractDescriptorV1TLV =>
              () // todo not supported
          }
        case None => ()
      }
    }

    def handleFileChosen(file: File): Unit = {
      val hex = Files.readAllLines(file.toPath).get(0)
      val acceptMessage = LnMessageFactory(DLCAcceptTLV).fromHex(hex)
      showDetails(acceptMessage, isFromFile = true)
    }

    val fileChooser = DLCDialog.fileChooserButton(
      open = true,
      { file =>
        DLCDialog.acceptDLCFile = Some(file)
        DLCDialog.acceptFileChosenLabel.text = file.toString
        handleFileChosen(file)
      })

    val fromFileHBox = new HBox() {
      spacing = 5
      children = Vector(new Label("DLC Accept File"),
                        fileChooser,
                        DLCDialog.acceptFileChosenLabel)
    }

    dialog.dialogPane().content = new ScrollPane {
      margin = Insets(10)
      content = new VBox() {
        margin = Insets(10)
        spacing = 10
        children = Vector(acceptTFHBox, separatorHBox, fromFileHBox, gridPane)
      }
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
