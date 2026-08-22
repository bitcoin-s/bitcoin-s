package org.bitcoins.core.security

import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.tlv.{ContractDescriptorV0TLV, ValueIterator}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.util.Try

/** Security reproduction tests for the DLC/TLV parser.
  *
  * Each test asserts the CORRECT/SAFE behavior. Tests that reproduce a bug FAIL
  * until the bug is fixed. Do not modify these tests to match buggy behavior.
  */
class TlvParsingSecurityTest extends BitcoinSUnitTest {

  "ValueIterator.takeBigSizePrefixedList" must
    "not build a result vector proportional to an attacker-controlled count" in {
      // Finding 1 (High): ValueIterator.scala:67-72 materializes
      // `0.until(len.toInt).toVector` before any element is parsed.
      // Correct behavior: the declared count is checked against the
      // remaining bytes first, parsing fails, and no per-element work runs.
      val declaredCount = 3000000L // millions: wasteful, but no OOM
      val bytes =
        BigSizeUInt(declaredCount).bytes ++ ByteVector(0x01, 0x02, 0x03, 0x04)
      val iter = ValueIterator(bytes)

      var elementParses = 0
      val result = Try(iter.takeBigSizePrefixedList { () =>
        elementParses += 1
        ()
      })

      assert(
        result.isFailure,
        s"Parsing must fail: declared count $declaredCount exceeds the " +
          s"remaining bytes, but parsing succeeded with " +
          s"${result.toOption.map(_.length)} elements"
      )
      assert(
        elementParses == 0,
        s"The element parse function ran $elementParses times even though " +
          s"the declared count $declaredCount exceeds the remaining bytes")
    }

  it must
    "reject an oversized declared count before parsing any element" in {
      // Finding 1 (High): ValueIterator.scala:67-72 starts parsing elements
      // after an unvalidated attacker-controlled count.
      // Correct behavior: parsing fails before the first element parse runs.
      val declaredCount = 3000000L
      val iter = ValueIterator(BigSizeUInt(declaredCount).bytes) // 0 elements

      var elementParses = 0
      val result = Try(iter.takeBigSizePrefixedList { () =>
        elementParses += 1
        iter.takeU16() // minimal element read, throws when out of bytes
      })

      assert(result.isFailure,
             "Parsing must fail when the declared count exceeds the " +
               "remaining bytes")
      assert(
        elementParses == 0,
        s"Element parsing started ($elementParses parse attempts) before " +
          s"the declared count $declaredCount was checked")
    }

  "ContractDescriptorV0TLV" must
    "fail when the declared outcome count exceeds the value bytes" in {
      // Finding 1 (High): caller at TLV.scala:1039 uses
      // ValueIterator.takeBigSizePrefixedList (ValueIterator.scala:67-72).
      // Correct behavior: a value that declares millions of outcomes but
      // contains none must fail to parse instead of trusting the count.
      val declaredCount = 3000000L
      val value = BigSizeUInt(declaredCount).bytes // count only, 0 outcomes

      val result = Try(ContractDescriptorV0TLV.fromTLVValue(value))

      assert(
        result.isFailure,
        s"Parsing must fail: declared outcome count $declaredCount exceeds " +
          s"the ${value.length} value bytes")
    }
}
