package org.bitcoins.lnd.rpc

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.testkit.fixtures.DualLndFixture

import scala.concurrent.duration.DurationInt

class LndRpcClientPairTest extends DualLndFixture {

  it must "get info from both lnds" in { param =>
    val (_, lndA, lndB) = param

    for {
      infoA <- lndA.getInfo
      infoB <- lndB.getInfo
    } yield {
      assert(infoA.identityPubkey != infoB.identityPubkey)
      assert(infoA.blockHeight >= 0)
      assert(infoB.blockHeight >= 0)
    }
  }

  it must "pay a invoice" in { param =>
    val (_, lndA, lndB) = param

    for {
      invoice <- lndA.addInvoice("test", Satoshis(100), 0)
      payment <- lndB.sendPayment(invoice.invoice)

      // Assert payment was successful
      _ = assert(payment.paymentError.isEmpty, payment.paymentError)

      _ <- AsyncUtil.awaitConditionF(() =>
        lndA.lookupInvoice(invoice.rHash).map(_.state.isSettled))
    } yield succeed
  }

  it must "monitor a invoice" in { param =>
    val (_, lndA, lndB) = param

    for {
      invoice <- lndA.addInvoice("test", Satoshis(100), 0)
      _ = system.scheduler.scheduleOnce(1.second) {
        lndB.sendPayment(invoice.invoice)
        ()
      }

      _ <- lndA.monitorInvoice(invoice.rHash)
    } yield succeed
  }

  it must "send outputs from one node to another" in { params =>
    val (bitcoind, lndA, lndB) = params

    val sendAmt = Satoshis(10000)
    val feeRate = SatoshisPerKW.fromLong(1000)

    for {
      oldBalA <- lndA.walletBalance().map(_.balance)
      oldBalB <- lndB.walletBalance().map(_.balance)

      addr <- lndB.getNewAddress
      output = TransactionOutput(sendAmt, addr.scriptPubKey)

      tx <- lndA.sendOutputs(Vector(output), feeRate, spendUnconfirmed = false)
      _ <- lndA.publishTransaction(tx)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))

      newBalA <- lndA.walletBalance().map(_.balance)
      newBalB <- lndB.walletBalance().map(_.balance)
    } yield {
      assert(newBalB == oldBalB + sendAmt)
      // account for variance in fees
      assert(newBalA === oldBalA - sendAmt - feeRate.calc(tx) +- Satoshis(6))
    }
  }
}
