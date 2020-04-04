package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.SignPTLC
import org.bitcoins.wallet.ptlc.PTLCMessage.PTLCAccept

object SignPTLCDialog
    extends PTLCDialog[SignPTLC](
      "Sign PTLC Refund",
      "Enter PTLCAccept message",
      Vector(PTLCDialog.acceptStr -> PTLCDialog.textArea())) {
  import PTLCDialog._

  override def constructFromInput(inputs: Map[String, String]): SignPTLC = {
    val accept = PTLCAccept.fromJson(ujson.read(inputs(acceptStr)))
    SignPTLC(accept, escaped = false)
  }
}
