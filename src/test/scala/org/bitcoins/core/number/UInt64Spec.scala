package org.bitcoins.core.number

import org.bitcoins.core.gen.NumberGenerator
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{ Prop, Properties }

import scala.util.Try

/**
 * Created by chris on 6/20/16.
 */
class UInt64Spec extends Properties("UInt64Spec") {

  property("Serialization symmetry") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      UInt64(uInt64.hex) == uInt64
      UInt64(uInt64.hex).hex == uInt64.hex
    }

  property("additive identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      if (uInt64.toBigInt <= UInt64.max.toBigInt) uInt64 + UInt64.zero == UInt64(uInt64.toBigInt)
      else uInt64 + UInt64.zero == uInt64
    }

  property("add two arbitrary uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      val result: BigInt = num1.toBigInt + num2.toBigInt
      if (result <= UInt64.max.toBigInt) num1 + num2 == UInt64(result)
      else Try(num1 + num2).isFailure
    }

  property("subtractive identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      if (uInt64.toBigInt <= UInt64.max.toBigInt) uInt64 - UInt64.zero == UInt64(uInt64.toBigInt)
      else uInt64 - UInt64.zero == uInt64
    }

  property("subtract a uint64 from a uint64") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      val result = num1.toBigInt - num2.toBigInt
      if (result < 0) Try(num1 - num2).isFailure
      else num1 - num2 == UInt64(result)
    }

  property("multiplying by zero") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      uInt64 * UInt64.zero == UInt64.zero
    }

  property("multiplicative identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      if (uInt64 == UInt64.zero) uInt64 * UInt64.one == UInt64.zero
      else uInt64 * UInt64.one == uInt64
    }

  property("multiply two uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      val result = num1.toBigInt * num2.toBigInt
      if (result <= UInt64.max.toBigInt) num1 * num2 == UInt64(result)
      else Try(num1 * num2).isFailure
    }

  property("< & >= for uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      if (num1.toBigInt < num2.toBigInt) num1 < num2
      else num1 >= num2
    }

  property("<= & > with two uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      if (num1.toBigInt <= num2.toBigInt) num1 <= num2
      else num1 > num2
    }

  property("== & != for two UInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      if (num1.toBigInt == num2.toBigInt) num1 == num2
      else num1 != num2
    }

  property("|") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      UInt64(num1.toBigInt | num2.toBigInt) == (num1 | num2)
    }

  property("&") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      UInt64(num1.toBigInt & num2.toBigInt) == (num1 & num2)
    }
}
