package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.tlv._

import org.bitcoins.gui.dlc.GlobalDLCData

import scalafx.scene.Node

class SignDLCDialog
    extends DLCDialog[SignDLCCliCommand]("Sign DLC",
                                         "Enter DLC Accept message",
                                         Vector(
                                           DLCDialog.dlcAcceptStr -> DLCDialog
                                             .textArea(),
                                           DLCDialog.dlcAcceptFileStr ->
                                             DLCDialog.fileChooserButton(
                                               open = true,
                                               { file =>
                                                 DLCDialog.acceptDLCFile =
                                                   Some(file)
                                                 DLCDialog.acceptFileChosenLabel.text =
                                                   file.toString
                                               }),
                                           DLCDialog.fileChosenStr -> DLCDialog.acceptFileChosenLabel
                                         ),
                                         Vector(DLCDialog.dlcAcceptStr,
                                                DLCDialog.dlcAcceptFileStr)) {

//  override def showAndWait(parentWindow: Window): Option[SignDLCCliCommand] = {
//    val dialog = new Dialog[Option[SignDLCCliCommand]]() {
//      initOwner(parentWindow)
//      title = "Sign DLC"
//      headerText = "Enter DLC Accept message"
//    }
//
//    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
//    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
//    dialog.resizable = true
//
//    var dlcDetailsShown = false
//    val acceptTLVTF = new TextField() {
//      minWidth = 300
//    }
//
//    var nextRow: Int = 2
//    val gridPane = new GridPane {
//      alignment = Pos.Center
//      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
//      hgap = 5
//      vgap = 5
//
//      add(new Label("DLC Accept"), 0, 0)
//      add(acceptTLVTF, 1, 0)
//    }
//
//    def showDLCTerms(status: DLCStatus): Unit = {
//      val descriptor = status.contractInfo.contractDescriptor.toTLV match {
//        case v0: ContractDescriptorV0TLV =>
//          EnumContractDescriptor
//            .fromTLV(v0)
//            .flip(status.totalCollateral.satoshis)
//        case _: ContractDescriptorV1TLV =>
//          throw new RuntimeException("This is impossible.")
//      }
//
//      val (oracleKey, eventId) = status.contractInfo.oracleInfo.toTLV match {
//        case OracleInfoV0TLV(announcement) =>
//          (announcement.publicKey.hex, announcement.eventTLV.eventId)
//        case _: MultiOracleInfoTLV =>
//          throw new RuntimeException("This is impossible.")
//      }
//
//      gridPane.add(new Label("Event Id"), 0, nextRow)
//      gridPane.add(
//        new TextField() {
//          text = eventId
//          editable = false
//        },
//        1,
//        nextRow
//      )
//      nextRow += 1
//
//      gridPane.add(new Label("Oracle Public Key"), 0, nextRow)
//      gridPane.add(
//        new TextField() {
//          text = oracleKey
//          editable = false
//        },
//        1,
//        nextRow
//      )
//      nextRow += 1
//
//      gridPane.add(new Label("Your Collateral"), 0, nextRow)
//      gridPane.add(new TextField() {
//                     text = status.localCollateral.satoshis.toString
//                     editable = false
//                   },
//                   1,
//                   nextRow)
//      nextRow += 1
//
//      gridPane.add(new Label("Counter Party Collateral"), 0, nextRow)
//      gridPane.add(new TextField() {
//                     text = status.remoteCollateral.satoshis.toString
//                     editable = false
//                   },
//                   1,
//                   nextRow)
//      nextRow += 1
//
//      gridPane.add(new Label("Potential Outcome"), 0, nextRow)
//      gridPane.add(new Label("Payouts"), 1, nextRow)
//      nextRow += 1
//
//      descriptor.foreach { case (str, satoshis) =>
//        gridPane.add(new TextField() {
//                       text = str.outcome
//                       editable = false
//                     },
//                     0,
//                     nextRow)
//        gridPane.add(new TextField() {
//                       text = satoshis.toString
//                       editable = false
//                     },
//                     1,
//                     nextRow)
//        nextRow += 1
//      }
//
//      gridPane.add(new Label("Fee Rate"), 0, nextRow)
//      gridPane.add(new TextField() {
//                     text = status.feeRate.toString
//                     editable = false
//                   },
//                   1,
//                   nextRow)
//      nextRow += 1
//
//      gridPane.add(new Label("Refund Date"), 0, nextRow)
//      gridPane.add(
//        new TextField() {
//          text = GUIUtil.epochToDateString(status.timeouts.contractTimeout)
//          editable = false
//        },
//        1,
//        nextRow)
//      nextRow += 1
//    }
//
//    acceptTLVTF.onKeyTyped = _ => {
//      if (!dlcDetailsShown) {
//        Try(
//          LnMessageFactory(DLCAcceptTLV).fromHex(
//            acceptTLVTF.text.value)) match {
//          case Failure(_) => ()
//          case Success(lnMessage) =>
//            val tempId = lnMessage.tlv.tempContractId
//            dlcs.find(_.tempContractId == tempId) match {
//              case Some(dlc) =>
//                dlc.contractInfo.contractDescriptor.toTLV match {
//                  case _: ContractDescriptorV0TLV =>
//                    dlcDetailsShown = true
//                    showDLCTerms(dlc)
//                    dialog.dialogPane().getScene.getWindow.sizeToScene()
//                  case _: ContractDescriptorV1TLV =>
//                    () // todo not supported
//                }
//              case None => ()
//            }
//        }
//      }
//    }
//
//    dialog.dialogPane().content = new ScrollPane {
//      content = new VBox(gridPane)
//    }
//
//    // When the OK button is clicked, convert the result to a SignDLC.
//    dialog.resultConverter = dialogButton =>
//      if (dialogButton == ButtonType.OK) {
//
//        val acceptHex = acceptTLVTF.text.value
//        val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptHex)
//
//        Some(SignDLC(accept))
//      } else None
//
//    val result = dialog.showAndWait()
//
//    result match {
//      case Some(Some(cmd: SignDLCCliCommand)) =>
//        Some(cmd)
//      case Some(_) | None => None
//    }
//  }

  override def constructFromInput(
      inputs: Predef.Map[String, Node]): SignDLCCliCommand = {
    DLCDialog.acceptDLCFile match {
      case Some(file) =>
        DLCDialog.acceptDLCFile = None // reset
        DLCDialog.acceptFileChosenLabel.text = "" // reset
        SignDLCFromFile(file.toPath, destination = None) //is none right here?
      case None =>
        val acceptHex = readStringFromNode(inputs(DLCDialog.dlcAcceptStr))

        val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptHex)

        //is this right???
        GlobalDLCData.lastContractId = accept.tlv.tempContractId.hex

        SignDLC(accept)

    }
  }
}
