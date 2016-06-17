package org.bitcoins.core.number

import org.bitcoins.core.util.NumberUtil
import org.scalacheck.Gen

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
}

object NumberGenerator extends NumberGenerator
