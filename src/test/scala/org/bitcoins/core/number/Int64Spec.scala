package org.bitcoins.core.number

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
      val result = num1.underlying + num2.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) num1 + num2 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) num1 + num2 == Int64(result)
      else Try(num1 + num2).isFailure
    }

  property("Add a int64 and a int32") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int32s) { (int64 : Int64, int32 : Int32) =>
      val result = int64.underlying + int32.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) int64 + int32 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) int64 + int32 == Int64(result)
      else Try(int64 + int32).isFailure
    }

  property("Subtractive identity") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 - Int64.zero == int64
    }

  property("Subtract two arbitrary int64s") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      val result = num1.underlying - num2.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) num1 - num2 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) num1 - num2 == Int64(result)
      else Try(num1 - num2).isFailure
    }

  property("Subtract a int64 and a int32") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int32s) { (int64 : Int64, int32 : Int32) =>
      val result = int64.underlying - int32.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) int64 - int32 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) int64 - int32 == Int64(result)
      else Try(int64 - int32).isFailure
    }

  property("Multiplying by zero") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 * Int64.zero == Int32.zero
    }

  property("Multiplicative identity") =
    Prop.forAll(NumberGenerator.int64s) { int64 : Int64 =>
      int64 * Int64.one == int64
    }

  property("Multiply two arbitrary int64s") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      val result = num1.underlying * num2.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) num1 * num2 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) num1 * num2 == Int64(result)
      else Try(num1 * num2).isFailure
    }

  property("Multiply a int64 and a int32") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int32s) { (int64 : Int64, int32 : Int32) =>
      val result = int64.underlying * int32.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) int64 * int32 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) int64 * int32 == Int64(result)
      else Try(int64 * int32).isFailure
    }

  property("<= & >") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      if (num1.underlying <= num2.underlying) num1 <= num2
      else num1 > num2

    }

  property("<= & > for int32 and int64") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int32s) { ( int64 : Int64, int32 : Int32 ) =>
      if (int64.underlying <= int32.underlying) int64 <= int32
      else int64 > int32
    }


  property("< & =>") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int64s) { (num1 : Int64, num2 : Int64) =>
      if (num1.underlying < num2.underlying) num1 < num2
      else num1 >= num2

    }

  property("< & >= for int32 and int64") =
    Prop.forAll(NumberGenerator.int64s, NumberGenerator.int32s) { (int64 : Int64, int32 : Int32) =>
      if (int64.underlying < int32.underlying) int64 < int32
      else int64 >= int32
    }

  property("== & !=") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2 : Int32) =>
      if (num1.underlying == num2.underlying) num1 == num2
      else num1 != num2
    }
}

