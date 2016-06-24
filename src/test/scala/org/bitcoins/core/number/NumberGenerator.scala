package org.bitcoins.core.number

import org.bitcoins.core.util.NumberUtil
import org.scalacheck.Gen._
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


  /**
    * Generates a number in the range 0 <= x <= 2 ^^32 - 1
    * then wraps it in a UInt32
    * @return
    */
  def uInt32s: Gen[UInt32] = Gen.choose(0L,(NumberUtil.pow2(32)-1).toLong).map(UInt32(_))


  /**
    * Chooses a BigInt in the ranges of 0 <= bigInt < 2^^64
    * @return
    */
  def bigInts : Gen[BigInt] = for {
    bigInt <- Arbitrary.arbBigInt.arbitrary
    exponent <- Gen.choose(1,2)
  } yield bigInt.abs.pow(exponent)

  def positiveBigInts : Gen[BigInt] = bigInts.filter(_ >= 0)

  def bigIntsUInt64Range : Gen[BigInt] = positiveBigInts.filter(_ < (BigInt(1) << 64))
  /**
    * Generates a number in the range 0 <= x < 2^^64
    * then wraps it in a UInt64
    * @return
    */
  def uInt64s : Gen[UInt64] = for {
    bigInt <- bigIntsUInt64Range
  } yield UInt64(bigInt)


  def int32s : Gen[Int32] = Gen.choose(Int32.min.underlying,Int32.max.underlying).map(Int32(_))

  def int64s : Gen[Int64] = Gen.choose(Int64.min.underlying, Int64.max.underlying).map(Int64(_))
}

object NumberGenerator extends NumberGenerator
