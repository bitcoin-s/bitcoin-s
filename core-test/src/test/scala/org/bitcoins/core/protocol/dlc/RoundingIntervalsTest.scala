package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.RoundingIntervals.IntervalStart
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen

class RoundingIntervalsTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "RoundingIntervals"

  it should "correctly not round" in {
    val anyLongGen = Gen.choose(Long.MinValue, Long.MaxValue)

    forAll(anyLongGen, anyLongGen) { case (outcome, payout) =>
      val roundedPayout =
        RoundingIntervals.noRounding.round(outcome, Satoshis(payout))

      assert(roundedPayout == Satoshis(payout))
    }
  }

  it should "correctly round" in {
    val roundingInterval = RoundingIntervals(
      Vector(
        IntervalStart(20, 2),
        IntervalStart(30, 3),
        IntervalStart(40, 4),
        IntervalStart(50, 5),
        IntervalStart(100, 10),
        IntervalStart(1000, 100)
      ))

    assert(roundingInterval.round(15, Satoshis(12345)) == Satoshis(12345))

    assert(roundingInterval.round(25, Satoshis(1234)) == Satoshis(1234))
    assert(roundingInterval.round(25, Satoshis(12345)) == Satoshis(12346))

    assert(roundingInterval.round(35, Satoshis(12345)) == Satoshis(12345))
    assert(roundingInterval.round(35, Satoshis(12346)) == Satoshis(12345))
    assert(roundingInterval.round(35, Satoshis(12347)) == Satoshis(12348))

    assert(roundingInterval.round(45, Satoshis(12344)) == Satoshis(12344))
    assert(roundingInterval.round(45, Satoshis(12345)) == Satoshis(12344))
    assert(roundingInterval.round(45, Satoshis(12346)) == Satoshis(12348))
    assert(roundingInterval.round(45, Satoshis(12347)) == Satoshis(12348))

    assert(roundingInterval.round(55, Satoshis(12345)) == Satoshis(12345))
    assert(roundingInterval.round(55, Satoshis(12346)) == Satoshis(12345))
    assert(roundingInterval.round(55, Satoshis(12347)) == Satoshis(12345))
    assert(roundingInterval.round(55, Satoshis(12348)) == Satoshis(12350))
    assert(roundingInterval.round(55, Satoshis(12349)) == Satoshis(12350))

    assert(roundingInterval.round(500, Satoshis(12340)) == Satoshis(12340))
    assert(roundingInterval.round(500, Satoshis(12341)) == Satoshis(12340))
    assert(roundingInterval.round(500, Satoshis(12344)) == Satoshis(12340))
    assert(roundingInterval.round(500, Satoshis(12345)) == Satoshis(12350))
    assert(roundingInterval.round(500, Satoshis(12346)) == Satoshis(12350))
    assert(roundingInterval.round(500, Satoshis(12349)) == Satoshis(12350))
    assert(roundingInterval.round(500, Satoshis(12350)) == Satoshis(12350))

    assert(roundingInterval.round(5000, Satoshis(12300)) == Satoshis(12300))
    assert(roundingInterval.round(5000, Satoshis(12301)) == Satoshis(12300))
    assert(roundingInterval.round(5000, Satoshis(12349)) == Satoshis(12300))
    assert(roundingInterval.round(5000, Satoshis(12350)) == Satoshis(12400))
    assert(roundingInterval.round(5000, Satoshis(12351)) == Satoshis(12400))
    assert(roundingInterval.round(5000, Satoshis(12399)) == Satoshis(12400))
    assert(roundingInterval.round(5000, Satoshis(12400)) == Satoshis(12400))
  }

  it should "correctly round on negative payouts" in {
    val roundingInterval = RoundingIntervals(
      Vector(
        IntervalStart(20, 2),
        IntervalStart(30, 3),
        IntervalStart(40, 4),
        IntervalStart(50, 5),
        IntervalStart(100, 10),
        IntervalStart(1000, 100)
      ))

    assert(roundingInterval.round(15, Satoshis(-12345)) == Satoshis(-12345))

    assert(roundingInterval.round(25, Satoshis(-1234)) == Satoshis(-1234))
    assert(roundingInterval.round(25, Satoshis(-12345)) == Satoshis(-12344))

    assert(roundingInterval.round(35, Satoshis(-12345)) == Satoshis(-12345))
    assert(roundingInterval.round(35, Satoshis(-12346)) == Satoshis(-12345))
    assert(roundingInterval.round(35, Satoshis(-12347)) == Satoshis(-12348))

    assert(roundingInterval.round(45, Satoshis(-12344)) == Satoshis(-12344))
    assert(roundingInterval.round(45, Satoshis(-12345)) == Satoshis(-12344))
    assert(roundingInterval.round(45, Satoshis(-12346)) == Satoshis(-12344))
    assert(roundingInterval.round(45, Satoshis(-12347)) == Satoshis(-12348))

    assert(roundingInterval.round(55, Satoshis(-12345)) == Satoshis(-12345))
    assert(roundingInterval.round(55, Satoshis(-12346)) == Satoshis(-12345))
    assert(roundingInterval.round(55, Satoshis(-12347)) == Satoshis(-12345))
    assert(roundingInterval.round(55, Satoshis(-12348)) == Satoshis(-12350))
    assert(roundingInterval.round(55, Satoshis(-12349)) == Satoshis(-12350))

    assert(roundingInterval.round(500, Satoshis(-12340)) == Satoshis(-12340))
    assert(roundingInterval.round(500, Satoshis(-12341)) == Satoshis(-12340))
    assert(roundingInterval.round(500, Satoshis(-12344)) == Satoshis(-12340))
    assert(roundingInterval.round(500, Satoshis(-12345)) == Satoshis(-12340))
    assert(roundingInterval.round(500, Satoshis(-12346)) == Satoshis(-12350))
    assert(roundingInterval.round(500, Satoshis(-12349)) == Satoshis(-12350))
    assert(roundingInterval.round(500, Satoshis(-12350)) == Satoshis(-12350))

    assert(roundingInterval.round(5000, Satoshis(-12300)) == Satoshis(-12300))
    assert(roundingInterval.round(5000, Satoshis(-12301)) == Satoshis(-12300))
    assert(roundingInterval.round(5000, Satoshis(-12349)) == Satoshis(-12300))
    assert(roundingInterval.round(5000, Satoshis(-12350)) == Satoshis(-12300))
    assert(roundingInterval.round(5000, Satoshis(-12351)) == Satoshis(-12400))
    assert(roundingInterval.round(5000, Satoshis(-12399)) == Satoshis(-12400))
    assert(roundingInterval.round(5000, Satoshis(-12400)) == Satoshis(-12400))
  }

  it should "correctly merge two RoundingIntervals" in {
    val roundingIntervals1 = RoundingIntervals(
      Vector(
        IntervalStart(2, 3),
        IntervalStart(4, 2),
        IntervalStart(5, 1),
        IntervalStart(6, 2),
        IntervalStart(7, 4),
        IntervalStart(8, 5),
        IntervalStart(9, 1),
        IntervalStart(10, 4),
        IntervalStart(12, 2),
        IntervalStart(13, 6),
        IntervalStart(14, 3),
        IntervalStart(15, 7),
        IntervalStart(16, 1)
      ))
    val roundingIntervals2 = RoundingIntervals(
      Vector(
        IntervalStart(1, 2),
        IntervalStart(3, 4),
        IntervalStart(4, 1),
        IntervalStart(5, 2),
        IntervalStart(6, 3),
        IntervalStart(11, 5)
      ))
    val expected = RoundingIntervals(
      Vector(
        IntervalStart(2, 2),
        IntervalStart(3, 3),
        IntervalStart(4, 1),
        IntervalStart(6, 2),
        IntervalStart(7, 3),
        IntervalStart(9, 1),
        IntervalStart(10, 3),
        IntervalStart(11, 4),
        IntervalStart(12, 2),
        IntervalStart(13, 5),
        IntervalStart(14, 3),
        IntervalStart(15, 5),
        IntervalStart(16, 1)
      ))

    assert(roundingIntervals1.minRoundingWith(roundingIntervals2) == expected)
  }
}
