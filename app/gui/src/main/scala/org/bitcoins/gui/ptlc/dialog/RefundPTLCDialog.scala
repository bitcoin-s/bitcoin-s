package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.RefundPTLC
import org.bitcoins.core.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object RefundPTLCDialog
    extends PTLCDialog[RefundPTLC](
      "Refund PTLC",
      "Enter Invoice ID",
      Vector(PTLCDialog.invoiceIdStr -> new TextField())) {
  override def constructFromInput(inputs: Map[String, String]): RefundPTLC = {
    RefundPTLC(Sha256DigestBE(inputs(PTLCDialog.invoiceIdStr)))
  }
}
