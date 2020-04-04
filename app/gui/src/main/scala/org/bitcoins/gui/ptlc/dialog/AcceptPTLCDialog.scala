package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.AcceptPTLC
import org.bitcoins.cli.PTLCMessage.PTLCInvoice

object AcceptPTLCDialog
    extends PTLCDialog[AcceptPTLC](
      "Accept PTLC Invoice",
      "Enter PTLC Information",
      Vector(PTLCDialog.invoiceStr -> PTLCDialog.textArea())) {
  import PTLCDialog._

  override def constructFromInput(inputs: Map[String, String]): AcceptPTLC = {
    val invoice = PTLCInvoice.fromJson(ujson.read(inputs(invoiceStr)))
    AcceptPTLC(invoice, escaped = false)
  }
}
