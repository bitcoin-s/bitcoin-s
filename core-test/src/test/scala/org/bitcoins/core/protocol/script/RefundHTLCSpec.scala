package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.HTLCGenerators
import org.scalacheck.{Prop, Properties}

class RefundHTLCSpec extends Properties("RefundHTLCSpec") {

  property("serialization symmetry") = {
    Prop.forAllNoShrink(HTLCGenerators.refundHTLC) { case (refundHTLC,privKeys) =>
      RefundHTLC(refundHTLC.bytes) == refundHTLC &&
        RefundHTLC(revocationKey = privKeys(0).publicKey,
          scriptNum = refundHTLC.locktime,
          delayedKey = privKeys(1).publicKey) == refundHTLC
    }
  }

}
