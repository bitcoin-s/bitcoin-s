package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.AcceptDLCOffer
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCOffer

object AcceptDLCDialog
    extends DLCDialog[AcceptDLCOffer](
      "Accept DLC Offer",
      "Enter DLC offer to accept",
      Vector(DLCDialog.dlcOfferStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): AcceptDLCOffer = {
    val offer = DLCOffer.fromJson(ujson.read(inputs(dlcOfferStr)))
    AcceptDLCOffer(offer, escaped = false)
  }
}
