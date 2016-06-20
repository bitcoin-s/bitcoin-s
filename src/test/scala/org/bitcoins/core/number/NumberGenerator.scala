package org.bitcoins.core.number

import org.bitcoins.core.util.NumberUtil
import org.scalacheck.{Arbitrary, Gen}

/**
  * Created by chris on 6/16/16.
  */
trait NumberGenerator {

  /**
    * Creates a generator that generates positive long numbers
    * @return
    */
  def positiveLongs: Gen[Long] = Gen.choose(0, Long.MaxValue)

  /**
    * Creates a generator for positive longs without the number zero
    * @return
    */
  def positiveLongsNoZero : Gen[Long] = Gen.choose(1,Long.MaxValue)

  /**
    * Creates a number generator that generates negative long numbers
    * @return
    */
  def negativeLongs: Gen[Long] = Gen.choose(Long.MinValue,-1)


  def bigInts : Gen[BigInt] = Arbitrary.arbBigInt.arbitrary

  /**
    * Generates a number in the range 0 <= x <= 2 ^^32 - 1
    * then wraps it in a UInt32
    * @return
    */
  def uInt32s: Gen[UInt32] = Gen.choose(0L,(NumberUtil.pow2(32)-1).toLong).map(UInt32(_))

  /**
    * Generates a number in the range 0 <= x < 2^^64
    * then wraps it in a UInt64
    * @return
    */
  def uInt64s : Gen[UInt64] = for {
    n <- Arbitrary.arbBigInt.arbitrary
    if n >= BigInt(0) && n < BigInt(2).pow(64)
  } yield UInt64(n)

}

object NumberGenerator extends NumberGenerator
