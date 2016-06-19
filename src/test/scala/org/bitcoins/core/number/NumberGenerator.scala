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

  def positiveLongsNoZero : Gen[Long] = Gen.choose(1,Long.MaxValue)

  /**
    * Creates a number generator that generates negative long numbers
    * @return
    */
  def negativeLongs: Gen[Long] = Gen.choose(Long.MinValue,-1)

  def uInt32s: Gen[UInt32] = Gen.choose(0L,(NumberUtil.pow2(32)-1).toLong).map(UInt32(_))

  def uInt64s : Gen[UInt64] = for {
    n <- Arbitrary.arbBigInt.arbitrary
    if n >= BigInt(0) && n < BigInt(2).pow(64)
  } yield UInt64(n)

}

object NumberGenerator extends NumberGenerator
