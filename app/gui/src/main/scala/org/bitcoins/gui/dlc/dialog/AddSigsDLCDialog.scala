package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.AddDLCSigs
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCSign

object AddSigsDLCDialog
    extends DLCDialog[AddDLCSigs](
      "Sign DLC",
      "Enter DLC signatures message",
      Vector(DLCDialog.dlcSigStr -> DLCDialog.textArea())) {

  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): AddDLCSigs = {
    val sign = DLCSign.fromJson(ujson.read(inputs(dlcSigStr)))
    AddDLCSigs(sign)
  }
}
