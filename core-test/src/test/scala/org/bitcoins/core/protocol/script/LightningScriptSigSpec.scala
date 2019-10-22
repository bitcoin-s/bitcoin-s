package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.HTLCGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

class LightningScriptSigSpec extends Properties("LightningScriptSigSpec") {
  private def logger = BitcoinSLogger.logger

  property("RefundHTLCScriptSig serialization symmetry") = {
    Prop.forAllNoShrink(HTLCGenerators.refundHTLCScriptSig) { case refund =>
      val first = RefundHTLCScriptSig(refund.hex) == refund
      val second = if (refund.isRevocation) {
        RefundHTLCScriptSig.createRevocation(refund.signatures.head) == refund
      } else {
        RefundHTLCScriptSig.createDelay(refund.signatures.head) == refund
      }
      first && second
    }
  }

  property("OfferedHTLCScriptSig serialization symmetry") = {
    Prop.forAllNoShrink(HTLCGenerators.offeredHTLCScriptSig) { case offered =>
      val first = OfferedHTLCScriptSig(offered.hex) == offered
      val reconstructed = offered.revocationOrPayment match {
        case Left((sig,key)) =>
          OfferedHTLCScriptSig(sig,key)
        case Right((preImage,sig)) =>
          OfferedHTLCScriptSig(preImage,sig)
      }
      val second = reconstructed == offered
      first && second
    }
  }

  property("ReceivedHTLCScriptSig serialization symmetry") = {
    Prop.forAllNoShrink(HTLCGenerators.receivedHTLCScriptSig) { case received =>
      val first = ReceivedHTLCScriptSig(received.hex) == received
      val reconstructed = received.timeoutOrRevocation match {
        case Left(sig) => ReceivedHTLCScriptSig(sig)
        case Right((sig,key)) => ReceivedHTLCScriptSig(sig,key)
      }
      val second = reconstructed == received
      first && second
    }
  }
}
