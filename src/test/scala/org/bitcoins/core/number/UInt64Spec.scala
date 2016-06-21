package org.bitcoins.core.number

import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/20/16.
  */
class UInt64Spec extends Properties("UInt64Spec") with BitcoinSLogger {


  property("Serialization symmetry") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      UInt64(uInt64.hex) == uInt64
      UInt64(uInt64.hex).hex == uInt64.hex
    }

  property("additive identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      if (uInt64.underlying <= UInt32.max.underlying) uInt64 + UInt64.zero == UInt32(uInt64.underlying.toLong)
      else uInt64 + UInt64.zero == uInt64
    }

  property("add two arbitrary uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      val result : BigInt = num1.underlying + num2.underlying
      if (result <= UInt32.max.underlying) num1 + num2 == UInt32(result.toLong)
      else if (result <= UInt64.max.underlying) num1 + num2 == UInt64(result)
      else Try(num1 + num2).isFailure
    }

  property("subtractive identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      if (uInt64.underlying <= UInt32.max.underlying) uInt64 - UInt64.zero == UInt32(uInt64.underlying.toLong)
      else  uInt64 - UInt64.zero == uInt64
    }

  property("subtract a uInt32 from a uInt64") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt32s) { (uInt64 : UInt64, uInt32 : UInt32) =>
      val result = uInt64.underlying - uInt32.underlying
      if (result < 0)  Try(uInt64 - uInt32).isFailure
      else if (result <= UInt32.max.underlying) uInt64 - uInt32 == UInt32(result.toLong)
      else uInt64 - uInt32 == UInt64(result)
    }

  property("subtract a uint64 from a uint64") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      val result = num1.underlying - num2.underlying
      if (result < 0)  Try(num1 - num2).isFailure
      else if (result <= UInt32.max.underlying) num1 - num2 == UInt32(result.toLong)
      else num1 - num2 == UInt64(result)
    }

  property("multiplying by zero") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      uInt64 * UInt64.zero == UInt32.zero
    }

  property("multiplicative identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      if (uInt64 == UInt64.zero) uInt64 * UInt64.one == UInt32.zero
      else if (uInt64.underlying <= UInt32.max.underlying) uInt64 * UInt64.one == UInt32(uInt64.underlying.toLong)
      else uInt64 * UInt64.one == uInt64
    }

  property("multiply uInt64 and a uInt32") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt32s) { (uInt64 : UInt64, uInt32 : UInt32) =>
    val result = uInt64.underlying * uInt32.underlying
    if (result <= UInt32.max.underlying) uInt64 * uInt32 == UInt32(result.toLong)
    else if (result <= UInt64.max.underlying) uInt64 * uInt32 == UInt64(result)
    else Try(uInt64 * uInt32).isFailure
  }

  property("multiply two uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      val result = num1.underlying * num2.underlying
      if (result <= UInt32.max.underlying) num1 * num2 == UInt32(result.toLong)
      else if (result <= UInt64.max.underlying) num1 * num2 == UInt64(result)
      else Try(num1 * num2).isFailure
    }

  property("< & >= for uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      if (num1.underlying < num2.underlying) num1 < num2
      else num1 >= num2
    }

  property("< & >= for a uInt64 and a uInt32") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt32s) { (num1: UInt64, num2: UInt32) =>
      if (num1.underlying < num2.underlying) num1 < num2
      else num1 >= num2
    }

  property("<= & > with two uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1: UInt64, num2: UInt64) =>
      if (num1.underlying <= num2.underlying) num1 <= num2
      else num1 > num2
    }

  property("<= & > for a uInt64 and a uInt32") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt32s) { (num1: UInt64, num2: UInt32) =>
      if (num1.underlying <= num2.underlying) num1 <= num2
      else num1 > num2
    }

  property("== & != for two UInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      if (num1.underlying == num2.underlying) num1 == num2
      else num1 != num2
    }
}
