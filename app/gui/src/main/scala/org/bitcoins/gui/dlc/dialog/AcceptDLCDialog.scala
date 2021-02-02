package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.dlc.{OracleInfo, SingleOracleInfo}
import org.bitcoins.core.protocol.tlv._
import scalafx.scene.Node
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, TextField}

class AcceptDLCDialog
    extends DLCDialog[AcceptDLCCliCommand](
      "Accept DLC Offer",
      "Enter DLC Offer to accept or open from file",
      Vector(
        DLCDialog.dlcOfferStr -> DLCDialog.textArea(),
        DLCDialog.dlcOfferFileStr -> DLCDialog.fileChooserButton(file => {
          DLCDialog.offerDLCFile = Some(file)
          DLCDialog.offerFileChosenLabel.text = file.toString
        }),
        DLCDialog.fileChosenStr -> DLCDialog.offerFileChosenLabel,
        DLCDialog.oracleAnnouncementStr -> new TextField() {
          promptText = "(optional)"
        }
      ),
      Vector(DLCDialog.dlcOfferStr,
             DLCDialog.dlcOfferFileStr,
             DLCDialog.oracleAnnouncementStr)) {
  import DLCDialog._

  def validateMatchingAnnouncement(
      offer: LnMessage[DLCOfferTLV],
      announcement: OracleAnnouncementTLV): Boolean = {
    val fromOffer = OracleInfo.fromTLV(offer.tlv.contractInfo.oracleInfo)
    val fromAnnouncement = SingleOracleInfo(announcement)

    fromOffer == fromAnnouncement
  }

  override def constructFromInput(
      inputs: Map[String, Node]): AcceptDLCCliCommand = {
    offerDLCFile match {
      case Some(file) =>
        // TODO figure how to validate when using a file
        offerDLCFile = None // reset
        offerFileChosenLabel.text = "" // reset
        AcceptDLCOfferFromFile(file.toPath)
      case None =>
        val offerHex = readStringFromNode(inputs(dlcOfferStr))
        val offer = LnMessageFactory(DLCOfferTLV).fromHex(offerHex)

        val announcementHex = readStringFromNode(inputs(oracleAnnouncementStr))

        if (announcementHex.nonEmpty) {
          val announcement = OracleAnnouncementTLV(announcementHex)
          if (!validateMatchingAnnouncement(offer, announcement)) {
            throw new RuntimeException(
              "Offer given does not have the same oracle info as announcement!")
          }
        } else {
          new Alert(AlertType.Confirmation) {
            resizable = true
            initOwner(owner)
            title = "Confirm no Oracle Announcement"
            contentText =
              s"Are you sure you would like sign to accept this DLC Offer without verifying it has the correct oracle?"
            dialogPane().getScene.getWindow.sizeToScene()
          }.showAndWait() match {
            case Some(ButtonType.OK) => ()
            case None | Some(_)      => throw new RuntimeException("Did not accept")
          }
        }

        AcceptDLCOffer(offer)
    }
  }
}
