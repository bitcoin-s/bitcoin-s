package org.bitcoins.crypto.security

import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Security reproduction tests for lax/unsafe parsing in the crypto module.
  * Each test asserts the CORRECT (safe) behavior, so every test FAILS until the
  * corresponding finding is fixed. The failing list is the fix checklist.
  */
class CryptoParsingSecurityTest extends BitcoinSCryptoTest {

  // r and s values with the top bit clear, taken from the existing
  // DERSignatureUtilTest lax-DER vectors
  private val rHex =
    "4c2dd8a9b6f8d425fcd8ee9a20ac73b619906a6367eac6cb93e70375225ec016"
  private val sHex =
    "356878eff111ff3663d7e6bf08947f94443845e0dcc54961664d922f7660b80c"

  private val expectedR: BigInt = BigInt(rHex, 16)
  private val expectedS: BigInt = BigInt(sHex, 16)

  behavior of "DERSignatureUtil.parseDERLax"

  it must "parse long-form length bytes the same way Bitcoin Core's lax DER parser does" in {
    // Finding: parseDERLax mis-parses long-form length bytes via signed Byte
    // arithmetic (DERSignatureUtil.scala:278,318). `lengthByte - 0x80` on a
    // signed Byte yields a negative count (0x81 -> -255) instead of Core's
    // `lenbyte & 0x7f` (0x81 -> 1). Correct behavior: parse like Bitcoin Core.

    // long-form total length byte 0x81, length stored in the next byte (0x44)
    val longTotalLengthSig =
      ByteVector.fromValidHex("308144" + "0220" + rHex + "0220" + sHex)
    DERSignatureUtil.parseDERLax(longTotalLengthSig) must be(
      Some((expectedR, expectedS)))

    // long-form r integer length byte 0x81, length stored in the next byte (0x20)
    val longRLengthSig = ByteVector.fromValidHex(
      "3045" + "02" + "81" + "20" + rHex + "0220" + sHex)
    DERSignatureUtil.parseDERLax(longRLengthSig) must be(
      Some((expectedR, expectedS)))
  }
}
