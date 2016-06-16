package org.bitcoins.core.number

import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/16/16.
  */
class UInt32Specification extends Properties("UInt32") with BitcoinSLogger {

  property("additive identity") = Prop.forAll(NumberGenerator.positiveLongs) { num : Long =>
    UInt32(num) + UInt32.zero == UInt32(num)
  }

  property("add 1") = Prop.forAll(NumberGenerator.positiveLongs) { num : Long =>
    UInt32(num) + UInt32.one == UInt32(num + 1)
  }

  property("Negative numbers in UInt32 throw an exception") = Prop.forAll(NumberGenerator.negativeLongs) { num : Long =>
    val uint32 = Try(UInt32(num))
    uint32.isFailure
  }

  property("add two uint32s and get the mathematical sum of the two numbers") =
    Prop.forAll(NumberGenerator.positiveLongs,NumberGenerator.positiveLongs) { (num1 : Long, num2 : Long) =>
      val uIntResult = UInt32(num1) + UInt32(num2)
      val expectedResult = BigInt(num1) + BigInt(num2)
      uIntResult.underlying == expectedResult
  }
}
