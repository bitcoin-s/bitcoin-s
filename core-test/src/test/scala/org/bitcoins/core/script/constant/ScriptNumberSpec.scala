package org.bitcoins.core.script.constant

import org.bitcoins.testkit.core.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by tom on 7/5/16.
  */
class ScriptNumberSpec extends Properties("ScriptNumberSpec") {
  property("Additive identity") = Prop.forAll(NumberGenerator.scriptNumbers) {
    num: ScriptNumber =>
      num + ScriptNumber.zero == num
  }
  property("Subtraction identity") =
    Prop.forAll(NumberGenerator.scriptNumbers) { num: ScriptNumber =>
      num - ScriptNumber.zero == num
    }
  property("Multiplicative identity") =
    Prop.forAll(NumberGenerator.scriptNumbers) { num: ScriptNumber =>
      num * ScriptNumber.one == num
    }
  property("< >=") =
    Prop.forAll(NumberGenerator.scriptNumbers, NumberGenerator.scriptNumbers) {
      (num1: ScriptNumber, num2: ScriptNumber) =>
        if (num1.toLong < num2.toLong) num1 < num2
        else num1 >= num2
    }
  property("> <=") =
    Prop.forAll(NumberGenerator.scriptNumbers, NumberGenerator.scriptNumbers) {
      (num1: ScriptNumber, num2: ScriptNumber) =>
        if (num1.toLong > num2.toLong) num1 > num2
        else num1 <= num2
    }
  property("== & !=") =
    Prop.forAll(NumberGenerator.scriptNumbers, NumberGenerator.scriptNumbers) {
      (num1: ScriptNumber, num2: ScriptNumber) =>
        if (num1.toLong == num2.toLong) num1 == num2
        else num1 != num2
    }
  property("add two script numbers") =
    Prop.forAll(NumberGenerator.scriptNumbers, NumberGenerator.scriptNumbers) {
      (num1: ScriptNumber, num2: ScriptNumber) =>
        num1 + num2 == ScriptNumber(num1.toLong + num2.toLong)
    }
  property("subtract a script number from another script number") =
    Prop.forAll(NumberGenerator.scriptNumbers, NumberGenerator.scriptNumbers) {
      (num1: ScriptNumber, num2: ScriptNumber) =>
        num1 - num2 == ScriptNumber(num1.toLong - num2.toLong)
    }
  property("multiply two script numbers") =
    Prop.forAll(NumberGenerator.scriptNumbers, NumberGenerator.scriptNumbers) {
      (num1: ScriptNumber, num2: ScriptNumber) =>
        num1 * num2 == ScriptNumber(num1.toLong * num2.toLong)
    }
  property("multiply a script number by zero should return zero") =
    Prop.forAll(NumberGenerator.scriptNumbers) { (num1: ScriptNumber) =>
      num1 * ScriptNumber.zero == ScriptNumber.zero
    }
}
