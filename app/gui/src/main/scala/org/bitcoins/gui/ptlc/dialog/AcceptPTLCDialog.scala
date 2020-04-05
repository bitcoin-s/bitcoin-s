package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.AcceptPTLC
import org.bitcoins.appCommons.JsonSerializers

object AcceptPTLCDialog
    extends PTLCDialog[AcceptPTLC](
      "Accept PTLC Invoice",
      "Enter PTLC Information",
      Vector(PTLCDialog.invoiceStr -> PTLCDialog.textArea())) {
  import PTLCDialog._

  override def constructFromInput(inputs: Map[String, String]): AcceptPTLC = {
    val invoice = JsonSerializers.getPTLCInvoice(ujson.read(inputs(invoiceStr)))
    AcceptPTLC(invoice, escaped = false)
  }
}
