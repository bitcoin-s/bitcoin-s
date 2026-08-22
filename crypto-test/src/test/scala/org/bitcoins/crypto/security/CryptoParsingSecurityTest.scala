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

  // generator point x coordinate, a known valid x-only public key
  private val validXOnlyHex =
    "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

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

  behavior of "Schnorr/XOnly key parsing"

  it must "reject wrong-length inputs instead of padding short inputs or dropping the first byte of 33-byte inputs" in {
    // Finding: SchnorrPublicKey.fromBytes, XOnlyPubKey.fromBytes and
    // SchnorrNonce.fromBytes silently pad inputs shorter than 32 bytes and
    // silently drop the first byte of 33-byte inputs
    // (SchnorrPublicKey.scala:79-95, XOnlyPubKey.scala:105-123,
    // SchnorrNonce.scala:21-23). Correct behavior: reject anything that is
    // not exactly 32 bytes.

    // 1 byte; when left-padded with zeros this is the valid x coordinate x=1,
    // so the current code silently accepts it
    val shortInput = ByteVector.fromValidHex("01")

    // 33 bytes: 0x02 prefix ++ a valid 32 byte x-only key; the current code
    // silently drops the first byte and accepts it
    val longInput = ByteVector.fromValidHex("02" + validXOnlyHex)

    // positive control: exactly 32 bytes must keep working
    val validInput = ByteVector.fromValidHex(validXOnlyHex)
    SchnorrPublicKey.fromBytes(validInput).bytes must be(validInput)
    XOnlyPubKey.fromBytes(validInput).bytes must be(validInput)
    SchnorrNonce.fromBytes(validInput).bytes must be(validInput)

    intercept[IllegalArgumentException] {
      SchnorrPublicKey.fromBytes(shortInput)
    }
    intercept[IllegalArgumentException] {
      XOnlyPubKey.fromBytes(shortInput)
    }
    intercept[IllegalArgumentException] {
      SchnorrNonce.fromBytes(shortInput)
    }
    intercept[IllegalArgumentException] {
      SchnorrPublicKey.fromBytes(longInput)
    }
    intercept[IllegalArgumentException] {
      XOnlyPubKey.fromBytes(longInput)
    }
    intercept[IllegalArgumentException] {
      SchnorrNonce.fromBytes(longInput)
    }
  }
}
