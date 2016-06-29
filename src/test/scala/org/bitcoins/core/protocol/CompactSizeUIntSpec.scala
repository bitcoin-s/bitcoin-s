package org.bitcoins.core.protocol

import org.bitcoins.core.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/29/16.
  */
class CompactSizeUIntSpec extends Properties("CompactSizeUIntSpec") {


  property("Serialization symmetry") =
    Prop.forAll(NumberGenerator.positiveLongs) { num : Long =>
      CompactSizeUInt.parseCompactSizeUInt(CompactSizeUInt(num).hex).num == num
    }
}
