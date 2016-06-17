package org.bitcoins.core.number

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
  def negativeLongs: Gen[Long] = Gen.choose(Long.MinValue, -1)
}

object NumberGenerator extends NumberGenerator
