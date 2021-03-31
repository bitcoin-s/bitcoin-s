package org.bitcoins.lnd.rpc

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.testkit.fixtures.DualLndFixture

import scala.concurrent.duration.DurationInt

class LndRpcClientPairTest extends DualLndFixture {

  it must "get info from both lnds" in { case (_, lndA, lndB) =>
    for {
      infoA <- lndA.getInfo
      infoB <- lndB.getInfo
    } yield {
      assert(infoA.identityPubkey != infoB.identityPubkey)
      assert(infoA.blockHeight >= 0)
      assert(infoB.blockHeight >= 0)
    }
  }

  it must "pay a invoice" in { case (_, lndA, lndB) =>
    for {
      invoice <- lndA.addInvoice("test", Satoshis(100), 0)
      payment <- lndB.sendPayment(invoice.invoice)

      // Assert payment was successful
      _ = assert(payment.paymentError.isEmpty, payment.paymentError)

      _ <- AsyncUtil.awaitConditionF(() =>
        lndA.lookupInvoice(invoice.rHash).map(_.state.isSettled))
    } yield succeed
  }

  it must "monitor a invoice" in { case (_, lndA, lndB) =>
    for {
      invoice <- lndA.addInvoice("test", Satoshis(100), 0)
      _ = system.scheduler.scheduleOnce(1.second) {
        lndB.sendPayment(invoice.invoice)
        ()
      }

      _ <- lndA.monitorInvoice(invoice.rHash)
    } yield succeed
  }
}
