package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.AddPTLCSig
import org.bitcoins.appCommons.JsonSerializers

object AddSigPTLCDialog
    extends PTLCDialog[AddPTLCSig](
      "Add PTLC Refund Signature",
      "Enter PTLCRefund Signature",
      Vector(PTLCDialog.refundSigStr -> PTLCDialog.textArea())) {
  import PTLCDialog._

  override def constructFromInput(inputs: Map[String, String]): AddPTLCSig = {
    val refundSig =
      JsonSerializers.getPTLCRefundSignature(ujson.read(inputs(refundSigStr)))
    AddPTLCSig(refundSig)
  }
}
