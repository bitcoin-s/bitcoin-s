package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.number.{Int32, UInt16, UInt32}
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.collection.immutable.NumericRange
import scala.math.Numeric.BigDecimalAsIfIntegral

class EventDescriptorTest extends BitcoinSUnitTest {

  behavior of "EventDescriptor"

  it must "create an enumerated event" in {
    val outcomes = Vector("Democrat_win", "Republican_win", "other")
    val enumEventDescriptorV0TLV = EnumEventDescriptorV0TLV(outcomes)

    assert(enumEventDescriptorV0TLV.outcomes == outcomes.map(Vector(_)))
    assert(enumEventDescriptorV0TLV.noncesNeeded == 1)
  }

  it must "create a range event" in {
    val rangeEventDescriptorV0TLV =
      RangeEventDescriptorV0TLV(start = Int32(-2),
                                count = UInt32(4),
                                step = UInt16.one,
                                unit = "test_unit",
                                precision = Int32.zero)

    assert(rangeEventDescriptorV0TLV.maxNum == 1)
    assert(rangeEventDescriptorV0TLV.minNum == -2)
    assert(
      rangeEventDescriptorV0TLV.outcomes == Vector("-2", "-1", "0", "1").map(
        Vector(_)))
    assert(
      rangeEventDescriptorV0TLV.outcomeNums
        .map(_.toInt) == Vector(-2, -1, 0, 1))

    val rangeEventBasePrecision1 =
      RangeEventDescriptorV0TLV(start = Int32(0),
                                count = UInt32(15),
                                step = UInt16.one,
                                unit = "test_unit",
                                precision = Int32(2))

    assert(rangeEventBasePrecision1.maxNum == 14)
    assert(rangeEventBasePrecision1.maxToPrecision == 1400)
    assert(rangeEventBasePrecision1.minNum == 0)
    assert(rangeEventBasePrecision1.minToPrecision == 0)
    val rangePrecision1 =
      NumericRange
        .inclusive[BigDecimal](start = 0, end = 1400, step = 100)(
          BigDecimalAsIfIntegral)
        .toVector
    assert(rangeEventBasePrecision1.outcomesToPrecision == rangePrecision1)
    assert(
      rangeEventBasePrecision1.outcomes == 0
        .until(15)
        .map(num => Vector(num.toString)))
  }

  it must "handle a range event with negative precision" in {
    //https://suredbits.slack.com/archives/CVA6LJA4E/p1604514328172300?thread_ts=1604507650.160900&cid=CVA6LJA4E
    val rangeEventBasePrecision1 =
      RangeEventDescriptorV0TLV(start = Int32(-10),
                                count = UInt32(20),
                                step = UInt16(5),
                                unit = "test_unit",
                                precision = Int32.negOne)
    val range =
      NumericRange[BigDecimal](start = -1.0, end = 9.0, step = 0.5)(
        BigDecimalAsIfIntegral).toVector
    assert(rangeEventBasePrecision1.stepToPrecision == 0.5)
    assert(rangeEventBasePrecision1.minNum == -10)
    assert(rangeEventBasePrecision1.minToPrecision == -1)
    assert(rangeEventBasePrecision1.maxNum == 85)
    assert(rangeEventBasePrecision1.maxToPrecision == 8.5)
    assert(rangeEventBasePrecision1.outcomesToPrecision == range)

    assert(
      rangeEventBasePrecision1.outcomes == -10
        .until(90, 5)
        .map(num => Vector(num.toString)))

    assert(!rangeEventBasePrecision1.containsToPrecision(8.4))
    assert(rangeEventBasePrecision1.containsToPrecision(8.5))
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

  it must "serialize and deserialize a range event" in {
    val hex = "fdd80815fffffffe0000000400010642544355534400000000"
    val re = RangeEventDescriptorV0TLV(start = Int32(-2),
                                       count = UInt32(4),
                                       step = UInt16.one,
                                       unit = "BTCUSD",
                                       precision = Int32.zero)

    val reFromHex = RangeEventDescriptorV0TLV.fromHex(hex)
    assert(reFromHex == re)
    assert(re.hex == hex)
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
    assert(descriptor.outcomeNums.map(_.toInt) == range)
    assert(descriptor.outcomes == range.map(num => Vector(num.toString)))

    val descriptor1 = descriptor.copy(numDigits = UInt16(2))
    assert(descriptor1.maxNum == 99)
    assert(descriptor1.minNum == 0)
    val expected1 = 0.until(100).toVector
    val expectedString1 = expected1.map { num =>
      String.format("%02d", num).toVector.map(_.toString)
    }
    assert(descriptor1.outcomeNums.map(_.toInt) == expected1)
    assert(descriptor1.outcomes == expectedString1)

    val descriptor2 = descriptor.copy(precision = Int32.negOne)

    assert(descriptor2.maxNum == 9)
    assert(descriptor2.maxToPrecision == 0.9)
    assert(descriptor2.minNum == 0)
    assert(descriptor2.minToPrecision == 0)
    val expectedString2 = 0.until(10).toVector.map(num => Vector(num.toString))
    assert(descriptor2.outcomes == expectedString2)
    assert(
      descriptor2.outcomesToPrecision.map(_.toDouble) == Vector(0.0, 0.1, 0.2,
        0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

    val descriptor3 =
      descriptor.copy(precision = Int32.negOne, numDigits = UInt16(2))

    assert(descriptor3.maxNum == 99)
    assert(descriptor3.maxToPrecision == 9.9)
    assert(descriptor3.minNum == 0)
    assert(descriptor3.minToPrecision == 0.0)
    val expected3 =
      NumericRange[BigDecimal](start = 0.0, end = 10.0, step = 0.1)(
        BigDecimalAsIfIntegral).toVector
    assert(descriptor3.outcomesToPrecision == expected3)

    val expectedStrings3: Vector[Vector[String]] =
      0.until(100).toVector.map { num =>
        String.format("%02d", num).toVector.map(_.toString)
      }

    assert(descriptor3.outcomes == expectedStrings3)
  }

  def formatNum(num: Int, numDigits: Int): Vector[String] = {
    val sign = if (num < 0) {
      "-"
    } else {
      "+"
    }

    val digits =
      String.format(s"%0${numDigits}d", Math.abs(num)).toVector.map(_.toString)

    sign +: digits
  }

  it must "create a signed digit decomposition event" in {
    val descriptor =
      SignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                              numDigits = UInt16(1),
                                              unit = "BTC/USD",
                                              precision = Int32.zero)

    val descriptorOutcomeNums = -9.until(10).toVector
    val descriptorOutcomes =
      descriptorOutcomeNums.map(formatNum(_, numDigits = 1))
    assert(descriptor.outcomes == descriptorOutcomes)
    assert(descriptor.outcomeNums.map(_.toInt) == descriptorOutcomeNums)

    val descriptor1 = descriptor.copy(precision = Int32.negOne)

    val expected =
      NumericRange[BigDecimal](start = -0.9, end = 1.0, step = 0.1)(
        BigDecimalAsIfIntegral).toVector

    assert(descriptor1.maxNum == 9)
    assert(descriptor1.maxToPrecision == 0.9)
    assert(descriptor1.minNum == -9)
    assert(descriptor1.minToPrecision == -0.9)
    assert(descriptor1.outcomesToPrecision == expected)
    assert(descriptor1.outcomes == descriptorOutcomes)

    val descriptor2 = descriptor1.copy(precision = Int32(-2))

    assert(descriptor2.minToPrecision == -0.09)
    assert(descriptor2.maxToPrecision == 0.09)
    val expected2 =
      NumericRange[BigDecimal](start = -0.09, end = 0.1, step = 0.01)(
        BigDecimalAsIfIntegral).toVector

    assert(descriptor2.outcomesToPrecision == expected2)
    assert(descriptor2.outcomes == descriptorOutcomes)

    val descriptor3 = descriptor2.copy(numDigits = UInt16(2))
    assert(descriptor3.minNum == -99)
    assert(descriptor3.minToPrecision == -0.99)
    assert(descriptor3.maxNum == 99)
    assert(descriptor3.maxToPrecision == 0.99)
    val expected3 =
      NumericRange[BigDecimal](start = -0.99, end = 1, step = 0.01)(
        BigDecimalAsIfIntegral).toVector

    assert(descriptor3.outcomesToPrecision == expected3)
    assert(
      descriptor3.outcomes == -99
        .until(100)
        .toVector
        .map(formatNum(_, numDigits = 2)))

    val descriptor4 =
      descriptor3.copy(numDigits = UInt16(3), precision = Int32(-1))

    assert(descriptor4.maxNum == 999)
    assert(descriptor4.maxToPrecision == 99.9)
    assert(descriptor4.minNum == -999)
    assert(descriptor4.minToPrecision == -99.9)

    val expected4 =
      NumericRange[BigDecimal](start = -99.9, end = 100, step = 0.1)(
        BigDecimalAsIfIntegral).toVector

    assert(descriptor4.outcomesToPrecision == expected4)
    assert(
      descriptor4.outcomes == -999
        .until(1000)
        .toVector
        .map(formatNum(_, numDigits = 3)))

    assert(!descriptor4.containsToPrecision(13.55))
    assert(descriptor4.containsToPrecision(13.5))
  }
}
