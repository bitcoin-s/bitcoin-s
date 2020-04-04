package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.BroadcastPTLC
import org.bitcoins.core.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object BroadcastPTLCDialog
    extends PTLCDialog[BroadcastPTLC](
      "Broadcast PTLC",
      "Enter Invoice ID",
      Vector(PTLCDialog.invoiceIdStr -> new TextField())) {
  override def constructFromInput(
      inputs: Map[String, String]): BroadcastPTLC = {
    BroadcastPTLC(Sha256DigestBE(inputs(PTLCDialog.invoiceIdStr)))
  }
}
