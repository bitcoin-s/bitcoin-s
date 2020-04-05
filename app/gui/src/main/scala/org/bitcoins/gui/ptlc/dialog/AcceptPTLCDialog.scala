package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.AcceptPTLC
import org.bitcoins.appCommons.JsonSerializers
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import scalafx.scene.control.TextField

object AcceptPTLCDialog
    extends PTLCDialog[AcceptPTLC](
      "Accept PTLC Invoice",
      "Enter PTLC Information",
      Vector(PTLCDialog.feeRateStr -> new TextField(),
             PTLCDialog.invoiceStr -> PTLCDialog.textArea()),
      Vector(PTLCDialog.feeRateStr)) {
  import PTLCDialog._

  override def constructFromInput(inputs: Map[String, String]): AcceptPTLC = {
    val invoice = JsonSerializers.getPTLCInvoice(ujson.read(inputs(invoiceStr)))
    val fee = if (inputs(PTLCDialog.feeRateStr).isEmpty) {
      None
    } else {
      Some(
        SatoshisPerVirtualByte(Satoshis(BigInt(inputs(PTLCDialog.feeRateStr)))))
    }
    AcceptPTLC(invoice, fee, escaped = false)
  }
}
