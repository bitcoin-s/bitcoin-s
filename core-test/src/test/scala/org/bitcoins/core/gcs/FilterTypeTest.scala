package org.bitcoins.core.gcs

import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class FilterTypeTest extends BitcoinSUnitTest {
  behavior of "FilterType"

  it must "parse bytes" in {
    assert(FilterType.fromBytes(ByteVector(0)) == FilterType.Basic)
    assertThrows[IllegalArgumentException](FilterType.fromBytes(ByteVector(1)))
  }

  it must "know its code" in {
    assert(FilterType.getCode(FilterType.Basic) == 0)
    assert(FilterType.byCode(0) == FilterType.Basic)
    assertThrows[IllegalArgumentException](FilterType.byCode(1))
    assertThrows[IllegalArgumentException](FilterType.getCode(FilterType.fromHex("ffff")))
  }
}
