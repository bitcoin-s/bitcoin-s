package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.CreateDLCOffer
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import scalafx.scene.Node

class OfferDLCDialog
    extends DLCDialog[CreateDLCOffer]("Create DLC Offer",
                                      "Enter DLC details",
                                      DLCDialog.constructOfferFields(),
                                      Vector(DLCDialog.feeRateStr)) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, Node]): CreateDLCOffer = {
    val feeRate = if (readStringFromNode(inputs(feeRateStr)).isEmpty) {
      None
    } else {
      Some(
        SatoshisPerVirtualByte(
          Satoshis(BigInt(readStringFromNode(inputs(feeRateStr))))
        )
      )
    }

    CreateDLCOffer(
      oracle = OracleAnnouncementV0TLV.fromHex(
        readStringFromNode(inputs(oracleAnnouncementStr))),
      contractInfo =
        ContractInfoTLV.fromHex(readStringFromNode(inputs(contractInfoStr))),
      collateral = Satoshis(BigInt(readStringFromNode(inputs(collateralStr)))),
      feeRateOpt = feeRate,
      locktime = UInt32(BigInt(readStringFromNode(inputs(locktimeStr)))),
      refundLT = UInt32(BigInt(readStringFromNode(inputs(refundLocktimeStr))))
    )
  }
}
