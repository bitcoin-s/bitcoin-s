package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency._
import org.bitcoins.core.number.Int64
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TxBuilderTest extends BitcoinSUnitTest {

  "TxBuilder" must "detect a bad fee on the tx" in {
    val estimatedFee = 1000.sats
    val actualFee = 1.sat
    val feeRate = SatoshisPerVirtualByte(1.sat)
    TxBuilder
      .isValidFeeRange(estimatedFee, actualFee, feeRate)
      .isFailure must be(true)

    TxBuilder
      .isValidFeeRange(actualFee, estimatedFee, feeRate)
      .isFailure must be(true)
  }
}
