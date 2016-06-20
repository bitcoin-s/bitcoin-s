package org.bitcoins.core.number

import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/20/16.
  */
class UInt64Specification extends Properties("UInt64Spec") with BitcoinSLogger {

  property("additive identity") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      uInt64 + UInt64.zero == uInt64
    }

  property("add 1") =
    Prop.forAll(NumberGenerator.uInt64s) { uInt64 : UInt64 =>
      uInt64 + UInt64.one == UInt64(uInt64.underlying + 1)
    }
}
