package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLC
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256Digest}
import scalafx.scene.control.TextField

object ForceCloseDLCDialog
    extends DLCDialog[ExecuteDLC](
      "DLC Close",
      "Enter DLC closing info",
      Vector(DLCDialog.dlcEventIdStr -> new TextField(),
             DLCDialog.dlcOracleSigStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): ExecuteDLC = {
    val eventId = Sha256Digest(inputs(dlcEventIdStr))
    val oracleSig = SchnorrDigitalSignature(inputs(dlcOracleSigStr))
    ExecuteDLC(eventId, oracleSig, noBroadcast = false)
  }
}
