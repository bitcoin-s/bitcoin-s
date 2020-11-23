package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.tlv._
import scalafx.scene.Node

object AcceptDLCDialog
    extends DLCDialog[AcceptDLCCliCommand](
      "Accept DLC Offer",
      "Enter DLC Offer to accept or open from file",
      Vector(
        DLCDialog.dlcOfferStr -> DLCDialog.textArea(),
        DLCDialog.dlcOfferFileStr -> DLCDialog.fileChooserButton(file => {
          DLCDialog.offerDLCFile = Some(file)
          DLCDialog.offerFileChosenLabel.text = file.toString
        }),
        DLCDialog.fileChosenStr -> DLCDialog.offerFileChosenLabel
      ),
      Vector(DLCDialog.dlcOfferStr, DLCDialog.dlcOfferFileStr)) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): AcceptDLCCliCommand = {
    offerDLCFile match {
      case Some(file) =>
        offerDLCFile = None // reset
        offerFileChosenLabel.text = "" // reset
        AcceptDLCOfferFromFile(file.toPath)
      case None =>
        val offerHex = readStringFromNode(inputs(dlcOfferStr))
        val offer = LnMessageFactory(DLCOfferTLV).fromHex(offerHex)

        AcceptDLCOffer(offer)
    }
  }
}
