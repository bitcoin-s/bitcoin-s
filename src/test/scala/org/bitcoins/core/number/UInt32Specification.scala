package org.bitcoins.core.number

import org.bitcoins.core.util.{BitcoinSLogger, NumberUtil}
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/16/16.
  */
class UInt32Specification extends Properties("UInt32") with BitcoinSLogger {


  property("serialization symmetry") = {
    Prop.forAll(NumberGenerator.uInt32s) { uInt32 : UInt32 =>
      UInt32(uInt32.hex) == uInt32
      UInt32(uInt32.hex).hex == uInt32.hex
    }
  }

  property("additive identity") = Prop.forAll(NumberGenerator.uInt32s) { num : UInt32 =>
    num + UInt32.zero == num
  }

  property("add 1") = Prop.forAll(NumberGenerator.uInt32s) { num : UInt32 =>
    num + UInt32.one == UInt32(num.underlying + 1)
  }

  property("Negative numbers in UInt32 throw an exception") = Prop.forAll(NumberGenerator.negativeLongs) { num : Long =>
    val uint32 = Try(UInt32(num))
    uint32.isFailure
  }

  property("add two uint32s and get the mathematical sum of the two numbers") =
    Prop.forAll(NumberGenerator.uInt32s,NumberGenerator.uInt32s) { (num1: UInt32, num2: UInt32) =>
      val uIntResult = Try(num1 + num2)
      val expectedResult = BigInt(num1.underlying) + BigInt(num2.underlying)
      if (expectedResult <= UInt32.max.underlying) uIntResult.get.underlying == expectedResult
      else uIntResult.isFailure
  }

  property("subtract zero from a UInt32 and get the original UInt32") =
    Prop.forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      uInt32 - UInt32.zero == uInt32
    }

  property("subtract one from a UInt32 and get the UInt32 - 1") =
    Prop.forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      (uInt32 - UInt32.one).underlying == (uInt32.underlying - 1)
    }

  property("subtract a uint32 from another uint32 and get the correct result") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) { (num1: UInt32, num2 : UInt32) =>
      if (num1.underlying >= num2.underlying) {
        (num1 - num2).underlying == num1.underlying - num2.underlying
      } else {
        //this will give us a negative number since num2 > num1
        //which should result in a failure
        Try(num1 - num2).isFailure
      }
    }

  property("subtract a uint64 from a uint32 and get the correct  result") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt64s) { (uInt32 : UInt32, uInt64 : UInt64) =>
      if (uInt32 >= uInt64) uInt32 - uInt64 == UInt32((uInt32.underlying - uInt64.underlying).toLong)
      else Try(uInt32 - uInt64).isFailure
    }

  property("multiplying by zero gives us zero") =
    Prop.forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      uInt32 * UInt32.zero == UInt32.zero
    }

  property("multiplicative identity") =
    Prop.forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      uInt32 * UInt32.one == uInt32
    }

  property("multiply two UInt32s") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) { (num1 : UInt32, num2: UInt32) =>
      val bigInt1 = BigInt(num1.underlying)
      val bigInt2 = BigInt(num2.underlying)
      if (bigInt1 * bigInt2 <= UInt32.max.underlying) {
        num1 * num2 == UInt32(num1.underlying * num2.underlying)
      } else if (bigInt1 * bigInt2 <= UInt64.max.underlying) {
        num1 * num2 == UInt64(BigInt(num1.underlying) * BigInt(num2.underlying))
      } else {
        Try(num1 * num2).isFailure
      }
    }

  property("< & >=") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) { (num1 : UInt32, num2 : UInt32) =>
      if (num1.underlying < num2.underlying) num1 < num2
      else num1 >= num2
    }

  property("<= & >") = {
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) { (num1: UInt32, num2: UInt32) =>
      if (num1.underlying <= num2.underlying) num1 <= num2
      else num1 > num2
    }
  }


  property("== & !=") = {
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) { (num1 : UInt32, num2 : UInt32) =>
      if (num1.underlying == num2.underlying) num1 == num2
      else num1 != num2
    }
  }



}
