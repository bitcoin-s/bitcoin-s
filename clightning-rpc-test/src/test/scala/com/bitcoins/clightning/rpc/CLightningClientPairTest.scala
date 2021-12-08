package com.bitcoins.clightning.rpc

import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.clightning.CLightningRpcTestUtil
import org.bitcoins.testkit.fixtures.DualCLightningFixture
import scodec.bits._

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util._

class CLightningClientPairTest extends DualCLightningFixture {

  it must "get info from both clightnings" in { param =>
    val (_, clightningA, clightningB) = param

    for {
      infoA <- clightningA.getInfo
      infoB <- clightningB.getInfo

      txs <- clightningA.listTransactions()
      funds <- clightningB.listFunds
    } yield {
      assert(infoA.id != infoB.id)
      assert(infoA.blockheight >= 0)
      assert(infoB.blockheight >= 0)

      assert(txs.nonEmpty)
      assert(funds.outputs.nonEmpty)
    }
  }

  it must "close a channel" in { param =>
    val (bitcoind, clightning, _) = param

    for {
      channels <- clightning.listChannels()
      channel = channels.head
      _ = assert(channel.active)

      closeResult <- clightning.closeChannel(channel.short_channel_id)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      tx <- bitcoind.getRawTransaction(closeResult.txid.get)
      find <- clightning.findChannel(channel.short_channel_id)
    } yield {
      assert(tx.confirmations.isDefined)
      assert(find.forall(!_.active))
    }
  }

  it must "sign a psbt" in { param =>
    val (bitcoind, clightning, _) = param

    for {
      addr <- clightning.getNewAddress
      _ <- bitcoind.sendToAddress(addr, Bitcoins(1))
      bitcoindAddr <- bitcoind.getNewAddress
      utxo <- clightning.listFunds.map(_.outputs.head)
      prevOut = TransactionOutput(utxo.value, utxo.scriptpubkey)

      input = TransactionInput(utxo.outPoint,
                               EmptyScriptSignature,
                               TransactionConstants.sequence)
      output = TransactionOutput(Bitcoins(0.5), bitcoindAddr.scriptPubKey)

      tx = BaseTransaction(Int32.two,
                           Vector(input),
                           Vector(output),
                           UInt32.zero)

      unsigned = PSBT
        .fromUnsignedTx(tx)
        .addWitnessUTXOToInput(prevOut, 0)

      // reserve first
      _ <- clightning.reserveInputs(unsigned)

      signed <- clightning.signPSBT(unsigned)
      finalized <- Future.fromTry(signed.finalizePSBT)
    } yield {
      finalized.extractTransactionAndValidate match {
        case Success(_)         => succeed
        case Failure(exception) => fail(exception)
      }
    }
  }

  it must "pay a invoice" in { param =>
    val (_, clightningA, clightningB) = param

    val amount = Satoshis(100)

    for {
      invoiceResult <- clightningA.createInvoice(amount = amount,
                                                 label = "label",
                                                 description = "description",
                                                 expirySeconds = 500)
      payment <- clightningB.payInvoice(invoiceResult.bolt11)
      _ = assert(payment.payment_hash == invoiceResult.payment_hash)
      _ = assert(payment.msatoshi.toSatoshis == amount)

      _ <- TestAsyncUtil.awaitConditionF(() =>
        clightningA
          .lookupInvoice(invoiceResult.payment_hash)
          .map(_.exists(_.status.paid)))
    } yield succeed
  }

  it must "wait for an invoice" in { param =>
    val (_, clightningA, clightningB) = param

    val amount = Satoshis(100)
    val label = "testLabel"

    for {
      invoiceResult <- clightningA.createInvoice(amount = amount,
                                                 label = label,
                                                 description = "description",
                                                 expirySeconds = 500)

      _ = system.scheduler.scheduleOnce(3.seconds) {
        clightningB.payInvoice(invoiceResult.bolt11)
        ()
      }

      res <- clightningA.waitInvoice(label)
    } yield assert(res.status.paid)
  }

  it must "send from one node to another" in { params =>
    val (bitcoind, clightningA, clightningB) = params

    val sendAmt = Satoshis(10000)
    val feeRate = SatoshisPerKW.fromLong(1000)

    for {
      oldBalA <- clightningA.walletBalance().map(_.balance)
      oldBalB <- clightningB.walletBalance().map(_.balance)

      addr <- clightningB.getNewAddress

      tx <- clightningA.sendToAddress(addr, sendAmt, feeRate).map(_.tx)
      _ <- bitcoind.broadcastTransaction(tx)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))

      // wait for them to sync the new blocks
      _ <- CLightningRpcTestUtil.awaitInSync(clightningA, bitcoind)
      _ <- CLightningRpcTestUtil.awaitInSync(clightningB, bitcoind)

      newBalA <- clightningA.walletBalance().map(_.balance)
      newBalB <- clightningB.walletBalance().map(_.balance)
    } yield {
      assert(newBalB == oldBalB + sendAmt)
      // account for variance in fees
      assert(newBalA === oldBalA - sendAmt - feeRate.calc(tx) +- Satoshis(200))
    }
  }

  it must "send a custom message to another peer" in { params =>
    val (_, clightningA, clightningB) = params

    for {
      nodeId <- clightningB.nodeId
      result <- clightningA.sendCustomMessage(
        nodeId,
        BigSizeUInt(48001),
        hex"000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
    } yield {
      assert(result.status.nonEmpty)
    }
  }
}
