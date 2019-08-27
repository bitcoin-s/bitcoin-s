package org.bitcoins.core.hd

import org.bitcoins.core.crypto.ExtKey
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.testkit.core.gen.{
  CryptoGenerators,
  HDGenerators,
  NumberGenerator
}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.{Gen, Shrink}
import org.scalatest.path

import scala.util.{Success, Try}
import org.bitcoins.core.crypto.ExtPrivateKey

class BIP32PathTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "BIP32Child"

  it must "fail to make children of out negative integers" in {
    forAll(NumberGenerator.negativeInts, Gen.oneOf(true, false)) { (i, bool) =>
      assertThrows[IllegalArgumentException](BIP32Node(i, bool))
    }
  }

  behavior of "BIP32Path"

  it must "derive children with the empty path" in {
    forAll(CryptoGenerators.extPrivateKey) { priv =>
      assert(priv.deriveChildPrivKey(BIP32Path.empty) == priv)
    }
  }

  it must "have varargs and vector constructors what work the same way" in {
    forAll(HDGenerators.bip32Path) { bip32 =>
      assert(BIP32Path(bip32.path) == BIP32Path(bip32.path: _*))
    }
  }

  it must "derive public keys" in {
    forAll(CryptoGenerators.extPrivateKey, HDGenerators.bip32Path) {
      (priv, path) =>
        val pub = priv.deriveChildPubKey(path)

        // we should always be able to derive pubkeys from privs, even with hard paths
        assert(pub.isSuccess)
    }
  }

  it must "derive public and private keys symmetrically" in {
    forAll(CryptoGenerators.extPrivateKey, HDGenerators.softBip32Path) {
      (priv, path) =>
        val derivedPubFromPriv: Try[ExtPublicKey] = priv.deriveChildPubKey(path)
        val pubFromDerivedPriv: ExtPublicKey =
          priv.deriveChildPrivKey(path).extPublicKey
        val derivedPubFromPub: Try[ExtPublicKey] =
          priv.extPublicKey.deriveChildPubKey(path)

        assert(Success(pubFromDerivedPriv) == derivedPubFromPriv)
        assert(Success(pubFromDerivedPriv) == derivedPubFromPub)
        assert(derivedPubFromPub == derivedPubFromPriv)
    }
  }

  it must "parse the empty path" in {
    val fromString = BIP32Path.fromString("m")
    assert(fromString == BIP32Path.empty)
  }

  it must "fail to parse a path beginning with the wrong character" in {
    forAll(HDGenerators.bip32Path, Gen.alphaChar.suchThat(_ != 'm')) {
      (path, char) =>
        val badPathString = char + path.toString.drop(1)
        assertThrows[IllegalArgumentException](
          BIP32Path.fromString(badPathString))
    }
  }

  it must "parse a hardened path" in {
    val fromString = BIP32Path.fromString("m/0'")
    assert(fromString.path.length == 1)
    assert(fromString.path.head.toUInt32 == ExtKey.hardenedIdx)
  }

  it must "parse the paths from the BIP32 test vectors" in {
    val expected1 = BIP32Path(
      Vector(BIP32Node(0, hardened = true), BIP32Node(1, hardened = false)))
    assert(BIP32Path.fromString("m/0'/1") == expected1)

    val expected2 = BIP32Path(
      Vector(BIP32Node(0, hardened = true),
             BIP32Node(1, hardened = false),
             BIP32Node(2, hardened = true)))
    assert(BIP32Path.fromString("m/0'/1/2'") == expected2)

    val expected3 = BIP32Path(
      Vector(BIP32Node(0, hardened = true),
             BIP32Node(1, hardened = false),
             BIP32Node(2, hardened = true)))
    assert(BIP32Path.fromString("m/0'/1/2'") == expected3)

    val expected4 = BIP32Path(
      Vector(BIP32Node(0, hardened = true),
             BIP32Node(1, hardened = false),
             BIP32Node(2, hardened = true),
             BIP32Node(2, hardened = false)))
    assert(BIP32Path.fromString("m/0'/1/2'/2") == expected4)

    val expected5 = BIP32Path(
      Vector(
        BIP32Node(0, hardened = true),
        BIP32Node(1, hardened = false),
        BIP32Node(2, hardened = true),
        BIP32Node(2, hardened = false),
        BIP32Node(1000000000, hardened = false)
      ))
    assert(BIP32Path.fromString("m/0'/1/2'/2/1000000000") == expected5)
  }

  it must "have fromString and toString symmetry" in {
    implicit val noShrink: Shrink[Nothing] = Shrink.shrinkAny
    forAll(HDGenerators.bip32Path) { path =>
      val toString = path.toString
      assert(path == BIP32Path.fromString(toString))
    }
  }

  it must "do path diffing" in {
    {
      val first = BIP32Path.fromString("m/44'/1'")
      assert(first.diff(first).contains(BIP32Path.empty))
    }

    {
      val first = BIP32Path.fromString("m/44'/0'/0'")
      val second = BIP32Path.fromString("m/44'/0'/0'/0/2")
      val expected = BIP32Path.fromString("m/0/2")
      assert(first.diff(second).contains(expected))
    }

    {
      val first = BIP32Path.fromString("m/44'/0'/0'/1")
      val second = BIP32Path.fromString("m/44'/0'/0'/1/2")
      val expected = BIP32Path.fromString("m/2")
      assert(first.diff(second).contains(expected))
    }

    {
      val first = BIP32Path.fromString("m/44'/1'")
      val second = BIP32Path.fromString("m/44'")
      assert(first.diff(second).isEmpty)
    }

    {
      val first = BIP32Path.fromString("m/44'")
      val second = BIP32Path.fromString("m/44'/1'")
      val expected = BIP32Path.fromString("m/1'")
      assert(first.diff(second).contains(expected))
    }

    {
      val first = BIP32Path.fromString("m/44'/1'")
      val second = BIP32Path.fromString("m/43'/2'")
      assert(first.diff(second).isEmpty)
    }

    {
      val first = BIP32Path.fromString("m/44'/1/0")
      val second = BIP32Path.fromString("m/44'/2/0")
      assert(first.diff(second).isEmpty)
    }

  }

  it must "do path diffing without altering the result" in {
    forAll(HDGenerators.diffableHDPaths, CryptoGenerators.extPrivateKey) {
      case ((short, long), xpriv) =>
        val diffed = short.diff(long) match {
          case None        => fail(s"$short and $long was not diffable!")
          case Some(value) => value
        }

        val longDerived = xpriv.deriveChildPrivKey(long)
        val diffDerived =
          xpriv.deriveChildPrivKey(short).deriveChildPrivKey(diffed)
        assert(longDerived == diffDerived)

    }
  }
}
