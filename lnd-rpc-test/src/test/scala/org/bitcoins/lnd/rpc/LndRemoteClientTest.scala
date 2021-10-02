package org.bitcoins.lnd.rpc

import lnrpc.Invoice.InvoiceState
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.testkit.fixtures.RemoteLndFixture

import scala.concurrent.Future

class LndRemoteClientTest extends RemoteLndFixture {

  it must "get info from lnd" in { lnd =>
    for {
      info <- lnd.getInfo
    } yield assert(info.blockHeight >= 0)
  }

  it must "create an invoice using sats" in { lnd =>
    val memo = "this is my memo"
    val amount = Satoshis(1000)

    for {
      invoiceResult <- lnd.addInvoice(memo, amount, 1000)
    } yield {
      val invoice = invoiceResult.invoice

      assert(invoice.lnTags.description.map(_.string).contains(memo))
      assert(invoice.amount.map(_.toSatoshis).contains(amount))
    }
  }

  it must "create an invoice using msats" in { lnd =>
    val memo = "this is my memo"
    val amount = MilliSatoshis(1000)

    for {
      invoiceResult <- lnd.addInvoice(memo, amount, 1000)
    } yield {
      val invoice = invoiceResult.invoice

      assert(invoice.lnTags.description.map(_.string).contains(memo))
      assert(invoice.amount.map(_.toMSat).contains(amount))
    }
  }

  it must "get an on-chain address" in { lnd =>
    for {
      addr <- lnd.getNewAddress
    } yield assert(addr.scriptPubKey.isInstanceOf[P2WPKHWitnessSPKV0])
  }

  it must "get unspent utxos" in { lnd =>
    for {
      utxos <- lnd.listUnspent
    } yield assert(utxos.isEmpty)
  }

  it must "look up an invoice" in { lnd =>
    val memo = "this is my memo"
    val amount = MilliSatoshis(1000)

    for {
      invoiceResult <- lnd.addInvoice(memo, amount, 1000)
      invoice = invoiceResult.invoice
      _ = {
        assert(invoice.lnTags.description.map(_.string).contains(memo))
        assert(invoice.amount.map(_.toMSat).contains(amount))
      }

      lookupResult <- lnd.lookupInvoice(invoiceResult.rHash)
    } yield {
      val lookupInvoice = LnInvoice.fromString(lookupResult.paymentRequest)

      assert(lookupInvoice.lnTags.description.map(_.string).contains(memo))
      assert(lookupInvoice.amount.map(_.toMSat).contains(amount))
      assert(lookupResult.state == InvoiceState.OPEN)
    }
  }

  it must "get wallet balance" in { lnd =>
    for {
      balances <- lnd.walletBalance()
    } yield {
      assert(balances.balance == Satoshis.zero)
      assert(balances.unconfirmedBalance == Satoshis.zero)
      assert(balances.confirmedBalance == Satoshis.zero)
    }
  }

  it must "get channel balance" in { lnd =>
    for {
      balances <- lnd.channelBalance()
    } yield {
      assert(balances.localBalance == Satoshis.zero)
      assert(balances.remoteBalance == Satoshis.zero)
      assert(balances.pendingOpenLocalBalance == Satoshis.zero)
      assert(balances.pendingOpenRemoteBalance == Satoshis.zero)
      assert(balances.unsettledLocalBalance == Satoshis.zero)
      assert(balances.unsettledRemoteBalance == Satoshis.zero)
    }
  }

  it must "lease and release an output" in { lnd =>
    for {
      utxos <- lnd.listUnspent
      leaseFs = utxos.map(u => lnd.leaseOutput(u.outPointOpt.get, 100))
      _ <- Future.sequence(leaseFs)
      leases <- lnd.listLeases()
      _ = assert(leases.size == utxos.size)

      releaseFs = utxos.map(u => lnd.releaseOutput(u.outPointOpt.get))
      _ <- Future.sequence(releaseFs)

      leases <- lnd.listLeases()
    } yield assert(leases.isEmpty)
  }
}
