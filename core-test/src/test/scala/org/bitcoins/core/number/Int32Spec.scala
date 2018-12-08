package org.bitcoins.core.number

import org.bitcoins.core.gen.NumberGenerator
import org.scalacheck.{ Prop, Properties }

import scala.util.Try

/**
 * Created by chris on 6/21/16.
 */
class Int32Spec extends Properties("Int32Spec") {

  property("Serialization symmetry") =
    Prop.forAll(NumberGenerator.int32s) { int32: Int32 =>
      Int32(int32.hex) == int32

    }
  property("Additive identity") =
    Prop.forAll(NumberGenerator.int32s) { int32: Int32 =>
      int32 + Int32.zero == int32
    }
  property("Add two arbitrary int32s") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      val result = num1.toLong + num2.toLong
      if (result <= Int32.max.toLong && result >= Int32.min.toLong) num1 + num2 == Int32(result)
      else Try(num1 + num2).isFailure
    }

  property("Subtractive identity") =
    Prop.forAll(NumberGenerator.int32s) { int32: Int32 =>
      int32 - Int32.zero == int32
    }

  property("Subtract two arbitrary int32s") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      val result = num1.toLong - num2.toLong
      if (result >= Int32.min.toLong && result <= Int32.max.toLong) num1 - num2 == Int32(result)
      else Try(num1 - num2).isFailure
    }

  property("Multiplying by zero") =
    Prop.forAll(NumberGenerator.int32s) { int32: Int32 =>
      int32 * Int32.zero == Int32.zero
    }

  property("Multiplicative identity") =
    Prop.forAll(NumberGenerator.int32s) { int32: Int32 =>
      int32 * Int32.one == int32
    }

  property("Multiply two int32s") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      val result = num1.toLong * num2.toLong
      if (result >= Int32.min.toLong && result <= Int32.max.toLong) num1 * num2 == Int32(result.toInt)
      else Try(num1 * num2).isFailure
    }

  property("<= & >") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      if (num1.toLong <= num2.toLong) num1 <= num2
      else num1 > num2

    }

  property("< & =>") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      if (num1.toLong < num2.toLong) num1 < num2
      else num1 >= num2

    }

  property("== & !=") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      if (num1.toLong == num2.toLong) num1 == num2
      else num1 != num2
    }

  property("|") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      Int32(num1.toLong | num2.toLong) == (num1 | num2)
    }

  property("&") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1: Int32, num2: Int32) =>
      Int32(num1.toLong & num2.toLong) == (num1 & num2)
    }

  property("negation") = {
    Prop.forAll(NumberGenerator.int32s) { int32 =>
      -int32 == Int32(-int32.toLong)
    }
  }
}
