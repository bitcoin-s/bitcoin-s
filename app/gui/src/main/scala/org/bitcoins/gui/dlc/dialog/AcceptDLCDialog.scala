package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.dlc.models.{OracleInfo, SingleOracleInfo}
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
        DLCDialog.dlcOfferFileStr -> DLCDialog.fileChooserButton(
          open = true,
          file => {
            DLCDialog.offerDLCFile = Some(file)
            DLCDialog.offerFileChosenLabel.text = file.toString
          }),
        DLCDialog.fileChosenStr -> DLCDialog.offerFileChosenLabel,
        DLCDialog.dlcAcceptFileDestStr -> DLCDialog.fileChooserButton(
          open = false,
          file => {
            DLCDialog.acceptDestDLCFile = Some(file)
            DLCDialog.acceptDestFileChosenLabel.text = file.toString
          }),
        DLCDialog.fileChosenStr -> DLCDialog.acceptDestFileChosenLabel,
        DLCDialog.oracleAnnouncementsStr -> new TextField() {
          promptText = "(optional)"
        }
      ),
      Vector(DLCDialog.dlcOfferStr,
             DLCDialog.dlcOfferFileStr,
             DLCDialog.dlcAcceptFileDestStr,
             DLCDialog.oracleAnnouncementsStr)) {
  import DLCDialog._

  def validateMatchingAnnouncements(
      offer: LnMessage[DLCOfferTLV],
      announcements: Vector[OracleAnnouncementTLV]): Boolean = {
    val fromOffer = OracleInfo.fromTLV(offer.tlv.contractInfo.oracleInfo)
    val singles = announcements.map(SingleOracleInfo(_))

    singles.forall(an => fromOffer.singleOracleInfos.contains(an))
  }

  override def constructFromInput(
      inputs: Map[String, Node]): AcceptDLCCliCommand = {
    offerDLCFile match {
      case Some(file) =>
        // TODO figure how to validate when using a file
        offerDLCFile = None // reset
        offerFileChosenLabel.text = "" // reset
        val destPathOpt = acceptDestDLCFile
        acceptDestDLCFile = None // reset
        acceptDestFileChosenLabel.text = "" // reset

        AcceptDLCOfferFromFile(file.toPath, destPathOpt.map(_.toPath))
      case None =>
        val offerHex = readStringFromNode(inputs(dlcOfferStr))
        val offer = LnMessageFactory(DLCOfferTLV).fromHex(offerHex)

        val announcementsHex = readStringFromNode(
          inputs(oracleAnnouncementsStr))

        if (announcementsHex.nonEmpty) {
          val announcements =
            announcementsHex.split(",").map(OracleAnnouncementTLV.fromHex)
          if (!validateMatchingAnnouncements(offer, announcements.toVector)) {
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
