package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.HTLCGenerators
import org.scalacheck.{Prop, Properties}

class OfferedHTLCSpec extends Properties("OfferedHTLCSpec") {

  property("serialization symmetry") = {
    Prop.forAllNoShrink(HTLCGenerators.offeredHTLC) { case (offeredHTLC, hashPreImage, privKeys) =>
      OfferedHTLC(offeredHTLC.hex) == offeredHTLC &&
        OfferedHTLC(privKeys(0).publicKey, privKeys(1).publicKey, privKeys(2).publicKey,
          offeredHTLC.paymentHash) == offeredHTLC
    }
  }
}
