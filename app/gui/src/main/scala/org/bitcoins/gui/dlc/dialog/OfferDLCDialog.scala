package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.CreateDLCOffer
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

object OfferDLCDialog
    extends DLCDialog[CreateDLCOffer](dialogTitle = "Create DLC Offer",
                                      header = "Enter DLC details",
                                      fields = DLCDialog.constructOfferFields(),
                                      optionalFields = Vector(
                                        DLCDialog.feeRateStr)) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): CreateDLCOffer = {
    val feeRate = if (inputs(feeRateStr).isEmpty) {
      None
    } else {
      Some(
        SatoshisPerVirtualByte(
          Satoshis(BigInt(inputs(feeRateStr)))
        )
      )
    }

    val oracleInfoHex = inputs(oraclePubKeyStr) ++ inputs(oracleNonceStr)

    CreateDLCOffer(
      oracleInfo = DLCMessage.OracleInfo.fromHex(oracleInfoHex),
      contractInfo = DLCMessage.ContractInfo.fromHex(inputs(contractInfoStr)),
      collateral = Satoshis(BigInt(inputs(collateralStr))),
      feeRateOpt = feeRate,
      locktime = UInt32(BigInt(inputs(locktimeStr))),
      refundLT = UInt32(BigInt(inputs(refundLocktimeStr))),
      escaped = false
    )
  }
}
