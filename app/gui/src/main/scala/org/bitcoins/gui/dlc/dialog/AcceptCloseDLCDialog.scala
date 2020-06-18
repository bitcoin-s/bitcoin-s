package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.AcceptDLCMutualClose
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCMutualCloseSig

object AcceptCloseDLCDialog
    extends DLCDialog[AcceptDLCMutualClose](
      "Accept DLC Close",
      "Enter mutual close offer",
      Vector(DLCDialog.dlcMutualCloseOfferStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): AcceptDLCMutualClose = {
    val mutualCloseSig =
      DLCMutualCloseSig.fromJson(ujson.read(inputs(dlcMutualCloseOfferStr)))
    AcceptDLCMutualClose(mutualCloseSig, noBroadcast = false)
  }
}
