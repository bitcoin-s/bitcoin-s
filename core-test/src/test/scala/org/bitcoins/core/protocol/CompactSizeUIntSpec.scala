package org.bitcoins.core.protocol

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/29/16.
  */
class CompactSizeUIntSpec extends Properties("CompactSizeUIntSpec") {

  property("Serialization symmetry") =
    Prop.forAll(NumberGenerator.compactSizeUInts) { compact: CompactSizeUInt =>
      CompactSizeUInt.parseCompactSizeUInt(compact.hex) == compact
    }
}
