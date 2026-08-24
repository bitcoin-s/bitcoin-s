package org.bitcoins.core.p2p

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

class FeeFilterMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.feeFilterMessage) { fee =>
      assert(FeeFilterMessage.fromBytes(fee.bytes) == fee)
    }
  }

  it must "parse the wire's little-endian feerate correctly, not double-reversed" in {
    // 999 sat/kb, little endian, as it appears on the wire. Round-trip
    // symmetry alone can't catch a double-reversal bug (both directions
    // would be equally wrong), so this asserts the actual decoded value.
    val feeFilterBytes = hex"e703000000000000"
    val feeFilterMessage = FeeFilterMessage.fromBytes(feeFilterBytes)

    feeFilterMessage.feeRate.currencyUnit.satoshis must be(Satoshis(999))
  }

  it must "parse a feefilter message with a feerate not divisible by 1000 without throwing" in {
    // a peer-supplied feerate has no guarantee of being evenly divisible by
    // 1000, so satPerByte must round rather than throw
    val feeFilterBytes = hex"e703000000000000"
    val feeFilterMessage = FeeFilterMessage.fromBytes(feeFilterBytes)

    val satPerByte = feeFilterMessage.satPerByte
    satPerByte.currencyUnit.satoshis must be(Satoshis(0))
  }
}
