package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.GetPTLC
import org.bitcoins.core.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object GetPTLCDialog
    extends PTLCDialog[GetPTLC]("Get PTLC",
                                "Enter Invoice ID",
                                Vector(
                                  PTLCDialog.invoiceIdStr -> new TextField())) {
  override def constructFromInput(inputs: Map[String, String]): GetPTLC = {
    GetPTLC(Sha256DigestBE(inputs(PTLCDialog.invoiceIdStr)))
  }
}
