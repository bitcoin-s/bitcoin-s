package org.bitcoins.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.crypto.signers.DSAKCalculator
import org.bouncycastle.util.{Arrays, BigIntegers}
import scodec.bits.ByteVector

/** Entirely copied from [[org.bouncycastle.crypto.signers.HMacDSAKCalculator HMacDSAKCalculator]]
  * with an added entropy parameter as well as two lines added adding the entropy to the hash.
  *
  * For a reference in secp256k1, see nonce_function_rfc6979 in secp256k1.c
  * For a description of the altered part, see RFC 6979 section 3.2d
  * here [[https://tools.ietf.org/html/rfc6979#section-3.2]]
  *
  * The added lines are marked below with comments.
  */
class HMacDSAKCalculatorWithEntropy(digest: Digest, entropy: ByteVector)
    extends DSAKCalculator {
  require(entropy.length == 32, "Entropy must be 32 bytes")

  private val ZERO = BigInteger.valueOf(0)

  private val hMac = new HMac(digest)
  private val K = new Array[Byte](hMac.getMacSize)
  private val V = new Array[Byte](hMac.getMacSize)

  private var n = CryptoParams.curve.getN

  override def isDeterministic = true

  override def init(n: BigInteger, random: SecureRandom): Unit = {
    throw new IllegalStateException("Operation not supported")
  }

  override def init(
      n: BigInteger,
      d: BigInteger,
      message: Array[Byte]): Unit = {
    this.n = n

    Arrays.fill(V, 0x01.toByte)
    Arrays.fill(K, 0.toByte)

    val x = new Array[Byte]((n.bitLength + 7) / 8)
    val dVal = BigIntegers.asUnsignedByteArray(d)

    System.arraycopy(dVal, 0, x, x.length - dVal.length, dVal.length)

    val m = new Array[Byte]((n.bitLength + 7) / 8)

    var mInt = bitsToInt(message)

    if (mInt.compareTo(n) >= 0)
      mInt = mInt.subtract(n)

    val mVal = BigIntegers.asUnsignedByteArray(mInt)

    System.arraycopy(mVal, 0, m, m.length - mVal.length, mVal.length)

    hMac.init(new KeyParameter(K))

    hMac.update(V, 0, V.length)
    hMac.update(0x00.toByte)
    hMac.update(x, 0, x.length)
    hMac.update(m, 0, m.length)
    hMac.update(entropy.toArray, 0, 32) // Added entropy

    hMac.doFinal(K, 0)

    hMac.init(new KeyParameter(K))

    hMac.update(V, 0, V.length)

    hMac.doFinal(V, 0)

    hMac.update(V, 0, V.length)
    hMac.update(0x01.toByte)
    hMac.update(x, 0, x.length)
    hMac.update(m, 0, m.length)
    hMac.update(entropy.toArray, 0, 32) // Added entropy

    hMac.doFinal(K, 0)

    hMac.init(new KeyParameter(K))

    hMac.update(V, 0, V.length)

    hMac.doFinal(V, 0)

    ()
  }

  override def nextK(): BigInteger = {
    val t = new Array[Byte]((n.bitLength + 7) / 8)

    while (true) {
      var tOff = 0
      while (tOff < t.length) {
        hMac.update(V, 0, V.length)

        hMac.doFinal(V, 0)

        val len = Math.min(t.length - tOff, V.length)
        System.arraycopy(V, 0, t, tOff, len)
        tOff += len
      }

      val k = bitsToInt(t)

      if (k.compareTo(ZERO) > 0 && k.compareTo(n) < 0)
        return k

      hMac.update(V, 0, V.length)
      hMac.update(0x00.toByte)

      hMac.doFinal(K, 0)

      hMac.init(new KeyParameter(K))

      hMac.update(V, 0, V.length)

      hMac.doFinal(V, 0)
    }

    throw new IllegalStateException("Return was supposed to happen above.")
  }

  private def bitsToInt(t: Array[Byte]) = {
    var v = new BigInteger(1, t)
    if (t.length * 8 > n.bitLength) v = v.shiftRight(t.length * 8 - n.bitLength)
    v
  }
}
