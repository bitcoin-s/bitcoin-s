package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.SignDLC
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCAccept
import org.bitcoins.gui.dlc.GlobalDLCData
import play.api.libs.json.Json

object SignDLCDialog
    extends DLCDialog[SignDLC](
      "Sign DLC",
      "Enter DLC accept message",
      Vector(DLCDialog.dlcAcceptStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): SignDLC = {
    val accept = DLCAccept.fromJson(Json.parse(inputs(dlcAcceptStr)))
    GlobalDLCData.lastEventId = accept.eventId.hex
    SignDLC(accept, escaped = false)
  }
}
