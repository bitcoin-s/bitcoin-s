package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.ClaimPTLC
import org.bitcoins.core.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object ClaimPTLCDialog
    extends PTLCDialog[ClaimPTLC](
      "Claim PTLC",
      "Enter Invoice ID",
      Vector(PTLCDialog.invoiceIdStr -> new TextField())) {
  override def constructFromInput(inputs: Map[String, String]): ClaimPTLC = {
    ClaimPTLC(Sha256DigestBE(inputs(PTLCDialog.invoiceIdStr)))
  }
}
