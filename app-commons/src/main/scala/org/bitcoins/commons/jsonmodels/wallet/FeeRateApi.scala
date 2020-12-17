package org.bitcoins.commons.jsonmodels.wallet

import org.bitcoins.core.wallet.fee.{
  SatoshisPerKiloByte,
  SatoshisPerVirtualByte
}

sealed abstract class FeeRateApiResult

case class BitcoinerLiveResult(
    timestamp: Long,
    estimates: Map[Int, BitcoinerLiveEstimate]
) extends FeeRateApiResult

case class BitcoinerLiveEstimate(sat_per_vbyte: SatoshisPerVirtualByte)

case class BitGoResult(
    feePerKb: SatoshisPerKiloByte,
    cpfpFeePerKb: SatoshisPerKiloByte,
    numBlocks: Int,
    confidence: Int,
    multiplier: Double,
    feeByBlockTarget: Map[Int, SatoshisPerKiloByte]
) extends FeeRateApiResult

case class MempoolSpaceResult(
    fastestFee: SatoshisPerVirtualByte,
    halfHourFee: SatoshisPerVirtualByte,
    hourFee: SatoshisPerVirtualByte
) extends FeeRateApiResult
