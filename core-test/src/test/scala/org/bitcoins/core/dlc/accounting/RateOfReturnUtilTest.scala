package org.bitcoins.core.dlc.accounting

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class RateOfReturnUtilTest extends BitcoinSUnitTest {
  behavior of "RateOfReturnUtil"

  it must "pretty print strings with percentages correctly" in {
    RateOfReturnUtil.prettyPrint(0) must be("0.00%")
    RateOfReturnUtil.prettyPrint(-1) must be("-100.00%")
    RateOfReturnUtil.prettyPrint(1) must be("100.00%")
    RateOfReturnUtil.prettyPrint(1.23) must be("123.00%")
    RateOfReturnUtil.prettyPrint(1.23456) must be("123.46%")
    RateOfReturnUtil.prettyPrint(1.23454) must be("123.45%")
  }
}
