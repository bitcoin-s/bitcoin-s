package org.bitcoins.lnd.rpc

import lnrpc.Invoice.InvoiceState
import lnrpc.{HopHint, Invoice, RouteHint}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.script.TaprootScriptPubKey
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkit.fixtures.LndFixture

import scala.concurrent.Future

class LndRpcClientTest extends LndFixture {

  it must "get info from lnd" in { lnd =>
    for {
      info <- lnd.getInfo
    } yield assert(info.blockHeight >= UInt32.zero)
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

  it must "create an invoice using a description hash" in { lnd =>
    val memo = "this is my memo"
    val hash = CryptoUtil.sha256(memo)
    val amount = Satoshis(1000)

    for {
      invoiceResult <- lnd.addInvoice(hash, amount, 1000)
    } yield {
      val invoice = invoiceResult.invoice

      assert(invoice.lnTags.descriptionHash.exists(_.hash == hash))
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

  it must "create an invoice and cancel it" in { lnd =>
    val memo = "this is my memo"
    val amount = Satoshis(1000)

    for {
      invoiceResult <- lnd.addInvoice(memo, amount, 1000)
      _ <- lnd.cancelInvoice(invoiceResult.invoice)
    } yield succeed
  }

  it must "get an on-chain address" in { lnd =>
    for {
      addr <- lnd.getNewAddress
    } yield assert(addr.scriptPubKey.isInstanceOf[TaprootScriptPubKey])
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

  it must "correctly handle UInt64 & UInt32 converters" in { lnd =>
    val hopHint = HopHint(
      nodeId =
        "037c862ec724bc85462aaeb804dd0941cd77dc4521cabd05edf3d0f23ed6b01f09",
      chanId = UInt64.max - UInt64.twentyTwo,
      feeBaseMsat = UInt32.zero,
      feeProportionalMillionths = UInt32.max,
      cltvExpiryDelta = UInt32.zero
    )

    val invoice = Invoice("memo",
                          value = 100000,
                          expiry = 1000,
                          routeHints = Vector(RouteHint(Vector(hopHint))))

    for {
      res <- lnd.addInvoice(invoice)
      lookup <- lnd.lookupInvoice(res.rHash)
    } yield {
      assert(lookup.routeHints == invoice.routeHints)
    }
  }
}
