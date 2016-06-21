package org.bitcoins.core.number

import org.scalacheck.{Prop, Properties}

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
    Prop.forAll(NumberGenerator.int32s)  { int32 : Int32 =>
      int32 + Int32.zero == int32
    }

  property("Add two arbitrary int32s") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2: Int32) =>
      val result = num1.underlying + num2.underlying
      if (result <= Int32.max.underlying) num1 + num2 == Int32(result)
      else if (result <= Int64.max.underlying) num1 + num2 == Int64(result)
      else Try(num1 + num2).isFailure
    }

  property("Add a int32 and a int64") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int64s) { (int32 : Int32, int64 : Int64) =>
      val result = int32.underlying + int64.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) int32 + int64 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) int32 + int64 == Int64(result)
      else Try(int32 + int64).isFailure
    }


  property("Subtractive identity") =
    Prop.forAll(NumberGenerator.int32s) { int32 : Int32 =>
      int32 - Int32.zero == int32
    }

  property("Subtract two arbitrary int32s") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2 : Int32) =>
      val result = num1.underlying - num2.underlying
      if (result >= Int32.min.underlying) num1 - num2 == Int32(result)
      else if (result >= Int64.min.underlying) num1 - num2 == Int64(result)
      else Try(num1 - num2).isFailure
    }

  property("Subtract a int32 and a int64") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int64s) { (int32 : Int32, int64 : Int64) =>
      val result = int32.underlying - int64.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) int32 - int64 == Int32(result.toInt)
      else if (result >= Int64.min.underlying && result <= Int64.max.underlying) int32 - int64 == Int64(result)
      else Try(int32 - int64).isFailure
    }

  property("Multiplying by zero") =
    Prop.forAll(NumberGenerator.int32s) { int32 : Int32 =>
      int32 * Int32.zero == Int32.zero
    }

  property("Multiplicative identity") =
    Prop.forAll(NumberGenerator.int32s) { int32: Int32 =>
      int32 * Int32.one == int32
    }

  property("Multiply two int32s") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2 : Int32) =>
      val result = num1.underlying * num2.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) num1 * num2 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) num1 * num2 == Int64(result)
      else Try(num1 * num2).isFailure
    }

  property("Multiply a int32 and a int64") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int64s) { (int32 : Int32, int64 : Int64) =>
      val result = int32.underlying * int64.underlying
      if (result >= Int32.min.underlying && result <= Int32.max.underlying) int32 * int64 == Int32(result.toInt)
      else if (result >= Int64.min.underlying  && result <= Int64.max.underlying) int32 * int64 == Int64(result)
      else Try(int32 * int64).isFailure
    }

  property("<= & >") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2 : Int32) =>
      if (num1.underlying <= num2.underlying) num1 <= num2
      else num1 > num2

    }

  property("<= & > for int32 and int64") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int64s) { (int32 : Int32, int64 : Int64) =>
      if (int32.underlying <= int64.underlying) int32 <= int64
      else int32 > int64
    }


  property("< & =>") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2 : Int32) =>
      if (num1.underlying < num2.underlying) num1 < num2
      else num1 >= num2

    }

  property("< & >= for int32 and int64") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int64s) { (int32 : Int32, int64 : Int64) =>
      if (int32.underlying < int64.underlying) int32 < int64
      else int32 >= int64
    }

  property("== & !=") =
    Prop.forAll(NumberGenerator.int32s, NumberGenerator.int32s) { (num1 : Int32, num2 : Int32) =>
      if (num1.underlying == num2.underlying) num1 == num2
      else num1 != num2
    }

}
