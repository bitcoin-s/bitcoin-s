package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.gen.ln.LnCurrencyUnitGen
import org.scalacheck.{ Prop, Properties }

import scala.util.Try

class LnCurrencyUnitSpec extends Properties("LnCurrencyUnitSpec") {

  property("Additive identity for LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      lnUnit + LnCurrencyUnits.zero == lnUnit
    }

  property("Add two LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      val result: Try[LnCurrencyUnit] = Try(num1 + num2)
      if (result.isSuccess && result.get >= PicoBitcoins.min &&
        result.get <= PicoBitcoins.max) num1 + num2 == result.get
      else Try(num1 + num2).isFailure
    }

  property("Subtractive identity for LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      lnUnit - LnCurrencyUnits.zero == lnUnit
    }

  property("Subtract two LnCurrencyUnit values") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      val result: Try[LnCurrencyUnit] = Try(num1 - num2)
      if (result.isSuccess && result.get >= PicoBitcoins.min &&
        result.get <= PicoBitcoins.max) num1 - num2 == result.get
      else Try(num1 - num2).isFailure
    }

  property("Multiply LnCurrencyUnit by zero") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      lnUnit * LnCurrencyUnits.zero == LnCurrencyUnits.zero
    }

  property("Multiplicative identity for LnCurrencyUnits") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      lnUnit * PicoBitcoins.one == lnUnit
    }

  property("Multiply two LnCurrencyUnit values") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      val result: Try[LnCurrencyUnit] = Try(num1 * num2)
      if (result.isSuccess && result.get >= PicoBitcoins.min &&
        result.get <= PicoBitcoins.max) num1 * num2 == result.get
      else Try(num1 * num2).isFailure
    }

  property("Convert negative LnCurrencyUnit value to Satoshis") =
    Prop.forAll(LnCurrencyUnitGen.negativeLnCurrencyUnit) { lnUnit =>
      lnUnit.toSatoshis <= Satoshis.zero
    }

  property("< & >=") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      (num1 < num2) || (num1 >= num2)
    }

  property("<= & >") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      (num1 <= num2) || (num1 > num2)
    }

  property("== & !=") =
    Prop.forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) { (num1: LnCurrencyUnit, num2: LnCurrencyUnit) =>
      (num1 == num2) || (num1 != num2)
    }
}

