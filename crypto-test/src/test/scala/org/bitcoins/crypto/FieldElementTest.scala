package org.bitcoins.crypto

class FieldElementTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "FieldElement"

  private val N = CryptoParams.getN

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
           CryptoGenerators.smallFieldElement) { case (fe1, fe2) =>
      val feSum = fe1.add(fe2).toBigInteger
      val bigIntSum = fe1.toBigInteger.add(fe2.toBigInteger)

      assert(feSum == bigIntSum)
    }
  }

  it must "add large numbers correctly" in {
    forAll(CryptoGenerators.largeFieldElement,
           CryptoGenerators.largeFieldElement) { case (fe1, fe2) =>
      val feSum = fe1.add(fe2).toBigInteger
      val bigIntSum = fe1.toBigInteger.add(fe2.toBigInteger).subtract(N)

      assert(feSum == bigIntSum)
    }
  }

  it must "subtract numbers correctly" in {
    forAll(CryptoGenerators.fieldElement, CryptoGenerators.fieldElement) {
      case (fe1, fe2) =>
        if (fe1.toBigInteger.compareTo(fe2.toBigInteger) > 0) {
          val feDiff = fe1.subtract(fe2).toBigInteger
          val bigIntDiff = fe1.toBigInteger.subtract(fe2.toBigInteger)

          assert(feDiff == bigIntDiff)
        } else {
          val feDiff = fe2.subtract(fe1).toBigInteger
          val bigIntDiff = fe2.toBigInteger.subtract(fe1.toBigInteger)

          assert(feDiff == bigIntDiff)
        }
    }
  }

  it must "wrap around correctly" in {
    assert(FieldElement.nMinusOne.add(FieldElement.one) == FieldElement.zero)
    assert(
      FieldElement.zero.subtract(FieldElement.one) == FieldElement.nMinusOne)
  }

  it must "multiply small numbers correctly" in {
    forAll(CryptoGenerators.reallySmallFieldElement,
           CryptoGenerators.reallySmallFieldElement) { case (fe1, fe2) =>
      val feProduct = fe1.multiply(fe2).toBigInteger
      val bigIntProduct = fe1.toBigInteger.multiply(fe2.toBigInteger)

      assert(feProduct == bigIntProduct)
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
