package org.bitcoins.core.crypto

import org.bitcoins.core.gen.CryptoGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Success

class ExtKeySpec extends Properties("ExtKeySpec") {
  private val logger = BitcoinSLogger.logger
  property("serialization symmetry") = {
    Prop.forAll(CryptoGenerators.extKey) { extKey =>
      ExtKey.fromString(extKey.toString) == Success(extKey) &&
      ExtKey(extKey.bytes) == extKey
    }
  }

  private def nonHardened: Gen[UInt32] =
    Gen.choose(0L, ((1L << 31) - 1)).map(UInt32(_))
  private def hardened: Gen[UInt32] =
    Gen.choose(1L << 31, (1L << 32) - 1).map(UInt32(_))

  property("derivation identity 1") = {
    Prop.forAllNoShrink(CryptoGenerators.extPrivateKey,
                        nonHardened,
                        nonHardened,
                        nonHardened) { (m, a, b, c) =>
      //https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#the-key-tree
      //N(m/a/b/c) = N(m/a/b)/c = N(m/a)/b/c = N(m)/a/b/c = M/a/b/c
      val path1 = m
        .deriveChildPrivKey(a)
        .deriveChildPrivKey(b)
        .deriveChildPrivKey(c)
        .extPublicKey
      val path2 = m
        .deriveChildPrivKey(a)
        .deriveChildPrivKey(b)
        .extPublicKey
        .deriveChildPubKey(c)
        .get
      val path3 = m
        .deriveChildPrivKey(a)
        .extPublicKey
        .deriveChildPubKey(b)
        .get
        .deriveChildPubKey(c)
        .get
      val path4 = m.extPublicKey
        .deriveChildPubKey(a)
        .get
        .deriveChildPubKey(b)
        .get
        .deriveChildPubKey(c)
        .get
      path1 == path2 && path2 == path3 && path3 == path4
    }
  }

  property("derivation identity 2") = {
    Prop.forAllNoShrink(CryptoGenerators.extPrivateKey,
                        hardened,
                        nonHardened,
                        nonHardened) { (m, aH, b, c) =>
      //https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#the-key-tree
      //N(m/aH/b/c) = N(m/aH/b)/c = N(m/aH)/b/c
      val path1 = m
        .deriveChildPrivKey(aH)
        .deriveChildPrivKey(b)
        .deriveChildPrivKey(c)
        .extPublicKey
      val path2 = m
        .deriveChildPrivKey(aH)
        .deriveChildPrivKey(b)
        .extPublicKey
        .deriveChildPubKey(c)
        .get
      val path3 = m
        .deriveChildPrivKey(aH)
        .extPublicKey
        .deriveChildPubKey(b)
        .get
        .deriveChildPubKey(c)
        .get
      path1 == path2 && path2 == path3
    }
  }
}
