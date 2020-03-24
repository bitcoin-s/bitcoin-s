package org.bitcoins.gui.dialog

import org.bitcoins.cli.CliCommand.SignDLC
import org.bitcoins.dlc.DLCMessage.DLCAccept
import org.bitcoins.gui.GlobalData

object SignDLCDialog
    extends DLCDialog[SignDLC](
      "Sign DLC",
      "Enter DLC accept message",
      Vector(DLCDialog.dlcAcceptStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): SignDLC = {
    val accept = DLCAccept.fromJson(ujson.read(inputs(dlcAcceptStr)))
    GlobalData.lastEventId = accept.eventId.hex
    SignDLC(accept, escaped = false)
  }
}
