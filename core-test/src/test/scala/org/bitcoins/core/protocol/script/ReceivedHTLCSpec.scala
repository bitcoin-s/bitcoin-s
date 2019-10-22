package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.HTLCGenerators
import org.scalacheck.{Prop, Properties}

class ReceivedHTLCSpec extends Properties("ReceivedHTLCSpec") {
  property("serialization symmetry") = {
    Prop.forAllNoShrink(HTLCGenerators.receivedHTLC) { case (receivedHTLC,privKeys) =>
      ReceivedHTLC(receivedHTLC.hex) == receivedHTLC &&
        ReceivedHTLC(privKeys(0).publicKey,privKeys(1).publicKey,receivedHTLC.paymentHash,
          privKeys(2).publicKey,receivedHTLC.lockTime) == receivedHTLC
    }
  }

}
