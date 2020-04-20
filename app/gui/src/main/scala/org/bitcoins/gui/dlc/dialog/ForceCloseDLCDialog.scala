package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLCUnilateralClose
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import scalafx.scene.control.TextField

object ForceCloseDLCDialog
    extends DLCDialog[ExecuteDLCUnilateralClose](
      "DLC Force Close",
      "Enter DLC closing info",
      Vector(DLCDialog.dlcEventIdStr -> new TextField(),
             DLCDialog.dlcOracleSigStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): ExecuteDLCUnilateralClose = {
    val eventId = Sha256DigestBE(inputs(dlcEventIdStr))
    val oracleSig = SchnorrDigitalSignature(inputs(dlcOracleSigStr))
    ExecuteDLCUnilateralClose(eventId, oracleSig, noBroadcast = false)
  }
}
