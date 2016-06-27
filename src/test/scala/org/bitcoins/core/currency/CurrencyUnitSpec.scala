package org.bitcoins.core.currency

import org.bitcoins.core.gen.CurrencyUnitGenerator
import org.scalacheck.{Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/23/16.
  */
class CurrencyUnitSpec extends Properties("CurrencyUnitSpec") {

  property("Symmetrical serialization for satoshis") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      Satoshis(satoshis.hex) == satoshis
    }

  property("additive identity") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis + CurrencyUnits.zero == satoshis
    }

  property("add two satoshis") =
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) { (num1 : Satoshis, num2 : Satoshis) =>
      val result = num1.underlying + num2.underlying
      if (result >= Satoshis.min.underlying &&
        result <= Satoshis.max.underlying) num1 + num2 == Satoshis(result)
      else Try(num1 + num2).isFailure
    }

  property("Subtractive identity for satoshis") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis - CurrencyUnits.zero == satoshis
    }

  property("Subtract two satoshi values") =
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) { (num1 : Satoshis, num2 : Satoshis) =>
      val result = num1.underlying - num2.underlying
      if (result >= Satoshis.min.underlying &&
        result <= Satoshis.max.underlying) num1 - num2 == Satoshis(result)
      else Try(num1 - num2).isFailure
    }

  property("Multiply satoshis by zero") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis * CurrencyUnits.zero == CurrencyUnits.zero
    }

  property("Multiplicative identity") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis * Satoshis.one == satoshis

    }

  property("Multiply two satoshi values") =
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) { (num1 : Satoshis, num2 : Satoshis) =>
      val result = num1.underlying * num2.underlying
      if (result >= Satoshis.min.underlying &&
        result <= Satoshis.max.underlying) num1 * num2 == Satoshis(result)
      else Try(num1 * num2).isFailure
    }

  property("< & >=") =
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) { (num1 : Satoshis, num2 : Satoshis) =>
      (num1 < num2) || (num1 >= num2)
    }

  property("<= & >") =
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) { (num1 : Satoshis, num2 : Satoshis) =>
      (num1 <= num2) || (num1 > num2)
    }

  property("== & !=") =
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) { (num1 : Satoshis, num2 : Satoshis) =>
      (num1 == num2) || (num1 != num2)
    }

}


