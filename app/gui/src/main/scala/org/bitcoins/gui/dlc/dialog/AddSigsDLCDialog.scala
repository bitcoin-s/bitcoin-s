package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.gui.dlc.GlobalDLCData
import scalafx.scene.Node

class AddSigsDLCDialog
    extends DLCDialog[AddDLCSigsCliCommand]("Add DLC Signatures",
                                            "Enter DLC signatures message",
                                            Vector(
                                              DLCDialog.dlcSigStr -> DLCDialog
                                                .textArea(),
                                              DLCDialog.dlcSignFileStr ->
                                                DLCDialog.fileChooserButton { file =>
                                                  DLCDialog.signDLCFile =
                                                    Some(file)
                                                  DLCDialog.signFileChosenLabel.text =
                                                    file.toString
                                                }
                                            ),
                                            Vector(DLCDialog.dlcSigStr,
                                                   DLCDialog.dlcSignFileStr)) {

  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): AddDLCSigsCliCommand = {
    signDLCFile match {
      case Some(file) =>
        signDLCFile = None // reset
        signFileChosenLabel.text = "" // reset
        AddDLCSigsFromFile(file.toPath)
      case None =>
        val signHex = readStringFromNode(inputs(dlcSigStr))

        val sign = LnMessageFactory(DLCSignTLV).fromHex(signHex)

        GlobalDLCData.lastContractId = sign.tlv.contractId.toHex

        AddDLCSigs(sign)
    }
  }
}
