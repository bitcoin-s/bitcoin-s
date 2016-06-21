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
      uInt64 + UInt64.zero == uInt64
    }

  property("add 1") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      if (uInt64 == UInt64.max) Try(uInt64 + UInt64.one).isFailure
      uInt64 + UInt64.one == UInt64(uInt64.underlying + 1)
    }

  property("add two arbitrary uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      val result : BigInt = num1.underlying + num2.underlying
      if (result <= UInt64.max.underlying) num1 + num2 == UInt64(result)
      else Try(num1 + num2).isFailure
    }

  property("subtractive identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      uInt64 - UInt64.zero == uInt64
      uInt64 - UInt32.zero == uInt64
    }

  property("subtract a uInt32 from a uInt64") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt32s) { (uInt64 : UInt64, uInt32 : UInt32) =>
      val result = uInt64.underlying - uInt32.underlying
      if (result >= 0) uInt64 - uInt32 == UInt64(result)
      else Try(uInt64 - uInt32).isFailure
    }

  property("subtract a uint64 from a uint64") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      val result = num1.underlying - num2.underlying
      if (result >= 0) num1 - num2 == UInt64(result)
      else Try(num1 - num2).isFailure
    }

  property("multiplying by zero") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      uInt64 * UInt64.zero == UInt64.zero
    }

  property("multiplicative identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      if (uInt64 == UInt64.zero) uInt64 * UInt64.one == UInt64.zero
      else uInt64 * UInt64.one == uInt64
    }

  property("multiply uInt64 and a uInt32") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt32s) { (uInt64 : UInt64, uInt32 : UInt32) =>
    val result = uInt64.underlying * uInt32.underlying
    if (result <= UInt64.max.underlying) uInt64 * uInt32 == UInt64(result)
    else Try(uInt64 * uInt32).isFailure
  }

  property("multiply two uInt64s") =
    Prop.forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) { (num1 : UInt64, num2 : UInt64) =>
      val result = num1.underlying * num2.underlying
      if (result <= UInt64.max.underlying) num1 * num2 == UInt64(result)
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
