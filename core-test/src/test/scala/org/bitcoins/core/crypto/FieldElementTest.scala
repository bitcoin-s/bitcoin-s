package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest

class FieldElementTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "FieldElement"

  private val N = CryptoParams.curve.getN

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.fieldElement) { fe =>
      assert(FieldElement(fe.bytes) == fe)
    }
  }

  it must "add zero correctly" in {
    forAll(CryptoGenerators.fieldElement) { fe =>
      assert(fe.add(FieldElement.zero) == fe)
      assert(FieldElement.zero.add(fe) == fe)
    }
  }

  it must "add small numbers correctly" in {
    forAll(CryptoGenerators.smallFieldElement,
           CryptoGenerators.smallFieldElement) {
      case (fe1, fe2) =>
        assert(
          fe1.add(fe2).toBigInteger == fe1.toBigInteger.add(fe2.toBigInteger))
    }
  }

  it must "add large numbers correctly" in {
    forAll(CryptoGenerators.largeFieldElement,
           CryptoGenerators.largeFieldElement) {
      case (fe1, fe2) =>
        assert(
          fe1
            .add(fe2)
            .toBigInteger == fe1.toBigInteger.add(fe2.toBigInteger).subtract(N))
    }
  }

  it must "subtract numbers correctly" in {
    forAll(CryptoGenerators.fieldElement, CryptoGenerators.fieldElement) {
      case (fe1, fe2) =>
        if (fe1.toBigInteger.compareTo(fe2.toBigInteger) > 0) {
          assert(
            fe1.subtract(fe2).toBigInteger == fe1.toBigInteger.subtract(
              fe2.toBigInteger))
        } else {
          assert(
            fe2.subtract(fe1).toBigInteger == fe2.toBigInteger.subtract(
              fe1.toBigInteger))
        }
    }
  }

  it must "wrap around correctly" in {
    val nMinusOne = FieldElement(
      "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140")
    assert(nMinusOne.add(FieldElement.one) == FieldElement.zero)
    assert(FieldElement.zero.subtract(FieldElement.one) == nMinusOne)
  }

  it must "multiply small numbers correctly" in {
    forAll(CryptoGenerators.reallySmallFieldElement,
           CryptoGenerators.reallySmallFieldElement) {
      case (fe1, fe2) =>
        assert(
          fe1.multiply(fe2).toBigInteger == fe1.toBigInteger.multiply(
            fe2.toBigInteger))
    }
  }

  it must "negate correctly" in {
    forAll(CryptoGenerators.fieldElement) { fe =>
      val negFe = fe.negate
      assert(fe.add(negFe) == FieldElement.zero)
    }
  }

  it must "invert correctly" in {
    forAll(CryptoGenerators.fieldElement) { fe =>
      val feInv = fe.inverse
      assert(fe.multiply(feInv) == FieldElement.one)
    }
  }
}
