package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.CreateDLCOffer
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import scalafx.scene.Node

class OfferDLCDialog
    extends DLCDialog[CreateDLCOffer](
      "Create DLC Offer",
      "Enter DLC details",
      DLCDialog.constructOfferFields(),
      Vector(DLCDialog.feeRateStr, DLCDialog.oracleThresholdStr)) {
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

    val contractInfo = {
      val totalCol = Satoshis(
        BigInt(readStringFromNode(inputs(totalCollateralStr))))
      val descriptor = ContractDescriptorTLV(
        readStringFromNode(inputs(contractDescriptorStr)))
      val announcementStrs =
        readStringFromNode(inputs(oracleAnnouncementsStr)).split(",")
      val announcementTLVs =
        announcementStrs.map(OracleAnnouncementV0TLV.fromHex)
      val threshold = readStringFromNode(inputs(oracleThresholdStr))

      val oracleInfo = if (announcementTLVs.length == 1) {
        OracleInfoV0TLV(announcementTLVs.head)
      } else {
        OracleInfoV1TLV(threshold.toInt, announcementTLVs.toVector)
      }

      ContractInfoV0TLV(totalCol, descriptor, oracleInfo)
    }

    CreateDLCOffer(
      contractInfo = contractInfo,
      collateral = Satoshis(BigInt(readStringFromNode(inputs(collateralStr)))),
      feeRateOpt = feeRate,
      locktime = UInt32(BigInt(readStringFromNode(inputs(locktimeStr)))),
      refundLT = UInt32(BigInt(readStringFromNode(inputs(refundLocktimeStr))))
    )
  }
}
