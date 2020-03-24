package org.bitcoins.gui.dialog

import org.bitcoins.cli.CliCommand.AcceptDLCOffer
import org.bitcoins.dlc.DLCMessage.DLCOffer

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
