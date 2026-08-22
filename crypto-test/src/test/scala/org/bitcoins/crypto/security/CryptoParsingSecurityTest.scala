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

  behavior of "SchnorrDigitalSignature.fromBytes"

  it must "reject a 65 byte signature with a trailing 0x00 sighash byte" in {
    // Finding: a 65-byte schnorr signature whose sighash byte is 0x00 is
    // accepted (SchnorrDigitalSignature.scala:34-43), violating BIP340/341
    // which require sig[64] != 0x00 for 65-byte signatures.
    // Correct behavior: reject the 0x00 trailing byte.

    // positive control: a plain 64 byte signature must keep working
    val sig64 = SchnorrDigitalSignature.dummy.bytes
    SchnorrDigitalSignature.fromBytes(sig64).bytes must be(sig64)

    val sig65WithZeroHashType = sig64.:+(0x00.toByte)
    intercept[IllegalArgumentException] {
      SchnorrDigitalSignature.fromBytes(sig65WithZeroHashType)
    }
  }

  behavior of "ECDigitalSignature.fromBytes"

  it must "not treat garbage bytes that silently decode to r=s=0 as a valid low-S or hash-typed signature" in {
    // Finding: arbitrary garbage bytes are silently accepted as an
    // ECDigitalSignature whose r and s both decode to 0
    // (DERSignatureUtil.scala:65-70, ECDigitalSignature.scala:77-85,131-147).
    //
    // ECDigitalSignature.fromBytes/apply itself must stay lenient and NOT
    // throw: it is called directly on untrusted, attacker-controlled
    // scriptSig bytes in consensus-critical script execution (e.g.
    // CryptoInterpreter.opCheckSig/opCheckMultiSig) with no Try wrapper, so
    // a malformed signature must fail verification cleanly rather than
    // crash script validation with an uncaught exception. Instead, the
    // derived accessors that silently treated an undecodable signature as
    // if it validly decoded to r=s=0 (isLowS, hashTypeOpt) must correctly
    // report that the bytes are not a valid DER signature.
    val garbageShort = ByteVector.fromValidHex("deadbeef")
    val garbageLong = ByteVector.fromValidHex(
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")

    // must not throw -- garbage bytes are still representable so callers
    // like the script interpreter can cleanly fail signature checks
    val sigShort = ECDigitalSignature.fromBytes(garbageShort)
    val sigLong = ECDigitalSignature.fromBytes(garbageLong)

    // r=s=0 is trivially "low S", but undecodable garbage must not be
    // reported as having a low (i.e. validly encoded) S value
    DERSignatureUtil.isLowS(sigShort) must be(false)
    DERSignatureUtil.isLowS(sigLong) must be(false)

    // there's no reliable way to locate a trailing hash type byte without a
    // validly decoded signature
    sigShort.hashTypeOpt must be(None)
    sigLong.hashTypeOpt must be(None)
  }
}
