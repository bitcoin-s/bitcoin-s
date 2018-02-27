package org.bitcoins.core.number

import org.bitcoins.core.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/21/16.
  */
class Int64Spec extends Properties("Int64Spec") {

  property("Symmetrical serialization") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      Int64(int64.hex) == int64
    }

  property("Additive identity") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 + Int64.zero == int64
    }
  property("Add two arbitrary int64s") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      val result = num1.toBigInt + num2.toBigInt
      if (result >= Int64.min.toLong  && result <= Int64.max.toLong) num1 + num2 == Int64(result)
      else Try(num1 + num2).isFailure
    }


  property("Subtractive identity") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 - Int64.zero == int64
    }

  property("Subtract two arbitrary int64s") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      val result = num1.toBigInt - num2.toBigInt
      if (result >= Int64.min.toLong  && result <= Int64.max.toLong) num1 - num2 == Int64(result)
      else Try(num1 - num2).isFailure
    }


  property("Multiplying by zero") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 * Int64.zero == Int64.zero
    }

  property("Multiplicative identity") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 * Int64.one == int64
    }

  property("Multiply two arbitrary int64s") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      val result = num1.toBigInt * num2.toBigInt
      if (result >= Int64.min.toLong && result <= Int64.max.toLong) num1 * num2 == Int64(result)
      else Try(num1 * num2).isFailure
    }

  property("<= & >") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      if (num1.toLong <= num2.toLong) num1 <= num2
      else num1 > num2

    }

  property("< & =>") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      if (num1.toLong < num2.toLong) num1 < num2
      else num1 >= num2

    }

  property("== & !=") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      if (num1.toLong == num2.toLong) num1 == num2
      else num1 != num2
    }

  property("|") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1: Int64, num2: Int64) =>
      Int64(num1.toLong | num2.toLong) == (num1 | num2)
    }

  property("&") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1: Int64, num2: Int64) =>
      Int64(num1.toLong & num2.toLong) == (num1 & num2)
    }

  property("negation") = {
    Prop.forAll(NumberGenerator.int64s) { int64 =>
      -int64 == Int64(-int64.toLong)
    }
  }

}

