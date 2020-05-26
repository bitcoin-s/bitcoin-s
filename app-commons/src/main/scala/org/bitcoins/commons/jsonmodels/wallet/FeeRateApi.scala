package org.bitcoins.commons.jsonmodels.wallet

import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

sealed abstract class FeeRateApiResult

case class BitcoinerLiveResult(
    timestamp: Long,
    estimates: Map[Int, BitcoinerLiveEstimate]
) extends FeeRateApiResult

case class BitcoinerLiveEstimate(sat_per_vbyte: SatoshisPerVirtualByte)
