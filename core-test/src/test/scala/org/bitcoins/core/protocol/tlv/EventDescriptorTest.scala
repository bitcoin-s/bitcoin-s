package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.number.{Int32, UInt16}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.collection.immutable.NumericRange
import scala.math.Numeric.BigDecimalAsIfIntegral

class EventDescriptorTest extends BitcoinSUnitTest {

  behavior of "EventDescriptor"

  it must "create an enumerated event" in {
    val outcomes = Vector("Democrat_win", "Republican_win", "other")
    val enumEventDescriptorV0TLV = EnumEventDescriptorV0TLV(outcomes)

    assert(enumEventDescriptorV0TLV.outcomes == outcomes)
    assert(enumEventDescriptorV0TLV.noncesNeeded == 1)
  }

  it must "be illegal to have num digits be zero" in {
    intercept[IllegalArgumentException] {
      UnsignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                                numDigits = UInt16.zero,
                                                unit = "BTC/USD",
                                                precision = Int32.zero)
    }
    intercept[IllegalArgumentException] {
      SignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                              numDigits = UInt16.zero,
                                              unit = "test_unit",
                                              precision = Int32.zero)
    }
  }
  it must "create a unsigned digit decomposition event" in {
    val descriptor =
      UnsignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                                numDigits = UInt16(1),
                                                unit = "BTC/USD",
                                                precision = Int32.zero)

    assert(descriptor.maxNum == 9)
    assert(descriptor.minNum == 0)
    val range = 0.until(10).toVector
    assert(range.forall(descriptor.containsPreciseOutcome(_)))
    assert(range.forall(descriptor.contains(_)))

    val descriptor1 = descriptor.copy(numDigits = UInt16(2))
    assert(descriptor1.maxNum == 99)
    assert(descriptor1.minNum == 0)
    val expected1 = 0.until(100).toVector
    assert(expected1.forall(descriptor1.containsPreciseOutcome(_)))
    assert(expected1.forall(descriptor1.contains(_)))

    val descriptor2 = descriptor.copy(precision = Int32.negOne)

    assert(descriptor2.maxNum == 9)
    assert(descriptor2.maxToPrecision == 0.9)
    assert(descriptor2.minNum == 0)
    assert(descriptor2.minToPrecision == 0)
    val expectedString2 = 0.until(10).toVector
    assert(expectedString2.forall(descriptor2.contains(_)))
    assert(
      Vector(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9).forall(
        descriptor2.containsPreciseOutcome(_)))

    val descriptor3 =
      descriptor.copy(precision = Int32.negOne, numDigits = UInt16(2))

    assert(descriptor3.maxNum == 99)
    assert(descriptor3.maxToPrecision == 9.9)
    assert(descriptor3.minNum == 0)
    assert(descriptor3.minToPrecision == 0.0)
    val expected3 =
      NumericRange[BigDecimal](start = 0.0, end = 10.0, step = 0.1)(
        BigDecimalAsIfIntegral).toVector
    assert(expected3.forall(descriptor3.containsPreciseOutcome(_)))

    val expectedOutcomes3: Vector[Int] =
      0.until(100).toVector

    assert(expectedOutcomes3.forall(descriptor3.contains(_)))
  }

  it must "create a signed digit decomposition event" in {
    val descriptor =
      SignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                              numDigits = UInt16(1),
                                              unit = "BTC/USD",
                                              precision = Int32.zero)

    val descriptorOutcomeNums: Vector[Int] = -9.until(10).toVector

    assert(descriptorOutcomeNums.forall(descriptor.contains(_)))
    assert(descriptorOutcomeNums.forall(descriptor.containsPreciseOutcome(_)))

    val descriptor1 = descriptor.copy(precision = Int32.negOne)

    val expected =
      NumericRange[BigDecimal](start = -0.9, end = 1.0, step = 0.1)(
        BigDecimalAsIfIntegral).toVector

    assert(descriptor1.maxNum == 9)
    assert(descriptor1.maxToPrecision == 0.9)
    assert(descriptor1.minNum == -9)
    assert(descriptor1.minToPrecision == -0.9)
    assert(expected.forall(descriptor1.containsPreciseOutcome(_)))
    assert(0.until(10).forall(descriptor1.contains(_)))

    val descriptor2 = descriptor1.copy(precision = Int32(-2))

    assert(descriptor2.minToPrecision == -0.09)
    assert(descriptor2.maxToPrecision == 0.09)
    val expected2 =
      NumericRange[BigDecimal](start = -0.09, end = 0.1, step = 0.01)(
        BigDecimalAsIfIntegral).toVector

    assert(expected2.forall(descriptor2.containsPreciseOutcome(_)))
    assert(-9.until(10).toVector.forall(descriptor2.contains(_)))

    val descriptor3 = descriptor2.copy(numDigits = UInt16(2))
    assert(descriptor3.minNum == -99)
    assert(descriptor3.minToPrecision == -0.99)
    assert(descriptor3.maxNum == 99)
    assert(descriptor3.maxToPrecision == 0.99)
    val expected3 =
      NumericRange[BigDecimal](start = -0.99, end = 1, step = 0.01)(
        BigDecimalAsIfIntegral).toVector

    assert(expected3.forall(descriptor3.containsPreciseOutcome(_)))
    assert(
      -99
        .until(100)
        .toVector
        .forall(descriptor3.contains(_)))

    val descriptor4 =
      descriptor3.copy(numDigits = UInt16(3), precision = Int32(-1))

    assert(descriptor4.maxNum == 999)
    assert(descriptor4.maxToPrecision == 99.9)
    assert(descriptor4.minNum == -999)
    assert(descriptor4.minToPrecision == -99.9)

    val expected4 =
      NumericRange[BigDecimal](start = -99.9, end = 100, step = 0.1)(
        BigDecimalAsIfIntegral).toVector

    assert(expected4.forall(descriptor4.containsPreciseOutcome(_)))
    assert(
      -999
        .until(1000)
        .toVector
        .forall(descriptor4.contains(_)))

    assert(!descriptor4.containsPreciseOutcome(13.55))
    assert(descriptor4.containsPreciseOutcome(13.5))
  }
}
