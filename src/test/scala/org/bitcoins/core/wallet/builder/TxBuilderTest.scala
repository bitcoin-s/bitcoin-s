package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.Int64
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.scalatest.{ FlatSpec, MustMatchers }

class TxBuilderTest extends FlatSpec with MustMatchers {

  "TxBuilder" must "detect a bad fee on the tx" in {
    val estimatedFee = Satoshis(Int64(1000))
    val actualFee = Satoshis.one
    val feeRate = SatoshisPerVirtualByte(Satoshis.one)
    TxBuilder.isValidFeeRange(estimatedFee, actualFee, feeRate) must be(Some(TxBuilderError.LowFee))

    TxBuilder.isValidFeeRange(actualFee, estimatedFee, feeRate) must be(Some(TxBuilderError.HighFee))
  }
}
