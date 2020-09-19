package org.bitcoins.core.number

import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/16/16.
  */
class UInt32Spec extends Properties("UInt32") with BitcoinSLogger {

  property("serialization symmetry") = {
    Prop.forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      UInt32(uInt32.hex) == uInt32
      UInt32(uInt32.hex).hex == uInt32.hex
    }
  }

  property("additive identity") = Prop.forAll(NumberGenerator.uInt32s) {
    num: UInt32 =>
      num + UInt32.zero == num
  }

  property("Negative numbers in UInt32 throw an exception") =
    Prop.forAll(NumberGenerator.negativeLongs) { num: Long =>
      val uint32 = Try(UInt32(num))
      uint32.isFailure
    }

  property("add two uint32s and get the mathematical sum of the two numbers") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result = num1.toLong + num2.toLong
        if (result <= UInt32.max.toLong) num1 + num2 == UInt32(result.toLong)
        else Try(num1 + num2).isFailure
    }

  property("subtractive identity") = Prop.forAll(NumberGenerator.uInt32s) {
    uInt32: UInt32 =>
      uInt32 - UInt32.zero == uInt32
  }

  property("subtract a uint32 from another uint32 and get the correct result") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result = num1.toLong - num2.toLong
        if (result >= 0) num1 - num2 == UInt32(result)
        else Try(num1 - num2).isFailure

    }

  property("multiplying by zero gives us zero") =
    Prop.forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      uInt32 * UInt32.zero == UInt32.zero
    }

  property("multiplicative identity") = Prop.forAll(NumberGenerator.uInt32s) {
    uInt32: UInt32 =>
      uInt32 * UInt32.one == uInt32
  }

  property("multiply two UInt32s") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val bigInt1 = num1.toBigInt
        val bigInt2 = num2.toBigInt
        if (bigInt1 * bigInt2 <= UInt32.max.toLong) {
          num1 * num2 ==
            UInt32(num1.toLong * num2.toLong)
        } else Try(num1 * num2).isFailure
    }

  property("< & >=") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        if (num1.toLong < num2.toLong) num1 < num2
        else num1 >= num2
    }

  property("<= & >") = {
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        if (num1.toLong <= num2.toLong) num1 <= num2
        else num1 > num2
    }
  }

  property("== & !=") = {
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        if (num1.toLong == num2.toLong) num1 == num2
        else num1 != num2
    }
  }

  property("|") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        UInt32(num1.toLong | num2.toLong) == (num1 | num2)
    }

  property("&") =
    Prop.forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        UInt32(num1.toLong & num2.toLong) == (num1 & num2)
    }

  property("<<") =
    Prop.forAllNoShrink(NumberGenerator.uInt32s, Gen.choose(0, 32)) {
      case (u32, shift) =>
        val r = Try(u32 << shift)
        val expected = (u32.toLong << shift) & 0xffffffffL
        if (r.isSuccess && expected <= UInt32.max.toLong) {
          r.get == UInt32(expected)
        } else {
          r.isFailure
        }
    }

  property(">>") = Prop.forAll(NumberGenerator.uInt32s, Gen.choose(0, 100)) {
    case (u32, shift) =>
      val r = u32 >> shift
      val expected = if (shift >= 64) 0 else u32.toLong >> shift
      r == UInt32(expected)
  }
}
