package org.bitcoins.lnd.rpc

import akka.stream._
import akka.stream.scaladsl._
import lnrpc._
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.{currencyUnitNumeric, Bitcoins, Satoshis}
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  P2WPKHWitnessSPKV0
}
import org.bitcoins.core.protocol.tlv.UnknownTLV
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.{SatoshisPerKW, SatoshisPerVirtualByte}
import org.bitcoins.crypto._
import org.bitcoins.testkit.fixtures.DualLndFixture
import scodec.bits.HexStringSyntax

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class LndRpcClientPairTest extends DualLndFixture {

  it must "get info from both lnds" in { param =>
    val (_, lndA, lndB) = param

    for {
      infoA <- lndA.getInfo
      infoB <- lndB.getInfo
    } yield {
      assert(infoA.identityPubkey != infoB.identityPubkey)
      assert(infoA.blockHeight >= UInt32.zero)
      assert(infoB.blockHeight >= UInt32.zero)
    }
  }

  it must "use the channel acceptor" in { param =>
    val (_, lndA, lndB) = param

    val (queue, source) =
      Source
        .queue[ChannelAcceptResponse](200, OverflowStrategy.dropHead)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()

    // for the test we'll only allow channels with a push amt
    lndA.lnd
      .channelAcceptor(source)
      .mapAsyncUnordered(1) { req =>
        if (req.pushAmt == UInt64.zero) {
          queue.offer(ChannelAcceptResponse(error = "give me money",
                                            pendingChanId = req.pendingChanId))
        } else {
          queue.offer(ChannelAcceptResponse(accept = true,
                                            pendingChanId = req.pendingChanId))
        }
      }
      .runWith(Sink.ignore)

    for {
      nodeId <- lndA.nodeId
      // reject channel with no push amount
      _ <- recoverToSucceededIf[Exception](
        lndB.openChannel(nodeId = nodeId,
                         fundingAmount = Satoshis(50000),
                         satPerVByte = SatoshisPerVirtualByte.one,
                         privateChannel = false))

      // accept channel with push amount
      outpointOpt <- lndB.openChannel(nodeId = nodeId,
                                      fundingAmount = Satoshis(50000),
                                      pushAmt = Satoshis(10),
                                      satPerVByte = SatoshisPerVirtualByte.one,
                                      privateChannel = false)

      pendingA <- lndA.listPendingChannels()
      pendingB <- lndB.listPendingChannels()
    } yield {
      assert(outpointOpt.isDefined)
      val outpoint = outpointOpt.get
      val expectedOutpoint = s"${outpoint.txId.hex}:${outpoint.vout.toInt}"

      assert(
        pendingA.pendingOpenChannels.exists(
          _.channel.get.channelPoint == expectedOutpoint))
      assert(
        pendingB.pendingOpenChannels.exists(
          _.channel.get.channelPoint == expectedOutpoint))
    }
  }

  it must "close a channel" in { param =>
    val (bitcoind, lnd, _) = param

    for {
      channels <- lnd.listChannels()
      channel = channels.head

      (txIdStr, voutStr) = channel.channelPoint.splitAt(
        channel.channelPoint.indexOf(":"))
      txId = DoubleSha256DigestBE(txIdStr)
      vout = UInt32(voutStr.tail.toLong)
      channelPoint = TransactionOutPoint(txId, vout)

      outPoint <- lnd.closeChannel(channelPoint)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      tx <- bitcoind.getRawTransaction(outPoint.txIdBE)
      find <- lnd.findChannel(channelPoint)
    } yield {
      assert(tx.confirmations.isDefined)
      assert(find.isEmpty)
    }
  }

  it must "sign a transaction" in { param =>
    val (bitcoind, lnd, _) = param

    for {
      addr <- lnd.getNewAddress(AddressType.WITNESS_PUBKEY_HASH)
      _ <- bitcoind.sendToAddress(addr, Bitcoins(1))
      bitcoindAddr <- bitcoind.getNewAddress
      utxo <- lnd.listUnspent.map(_.head)
      prevOut = TransactionOutput(utxo.amount, utxo.spk)

      input = TransactionInput(utxo.outPointOpt.get,
                               EmptyScriptSignature,
                               TransactionConstants.sequence)
      output = TransactionOutput(Bitcoins(0.5), bitcoindAddr.scriptPubKey)

      unsigned = BaseTransaction(Int32.two,
                                 Vector(input),
                                 Vector(output),
                                 UInt32.zero)

      (scriptSig, wit) <- lnd.computeInputScript(unsigned, 0, prevOut)
    } yield {
      val psbt = PSBT
        .fromUnsignedTx(unsigned)
        .addWitnessUTXOToInput(prevOut, 0)
        .addFinalizedScriptWitnessToInput(scriptSig, wit, 0)

      psbt.extractTransactionAndValidate match {
        case Success(_)         => succeed
        case Failure(exception) => fail(exception)
      }
    }
  }

  it must "pay an invoice" in { param =>
    val (_, lndA, lndB) = param

    for {
      invoice <- lndA.addInvoice("test", Satoshis(100), 0)
      payment <- lndB.sendPayment(invoice.invoice, 1.minute)

      // Assert payment was successful
      _ = assert(payment.status.isSucceeded)
      _ = assert(payment.failureReason.isFailureReasonNone)
      _ = assert(payment.htlcs.nonEmpty)

      _ <- AsyncUtil.awaitConditionF(() =>
        lndA.lookupInvoice(invoice.rHash).map(_.state.isSettled))
    } yield succeed
  }

  it must "monitor an invoice" in { param =>
    val (_, lndA, lndB) = param

    for {
      invoice <- lndA.addInvoice("test", Satoshis(100), 0)
      _ = system.scheduler.scheduleOnce(1.second) {
        lndB.sendPayment(invoice.invoice, 1.minute)
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

      detailsOpt <- lndB.getTransaction(tx.txIdBE)
      _ = assert(detailsOpt.isDefined)
      details = detailsOpt.get

      newBalA <- lndA.walletBalance().map(_.balance)
      newBalB <- lndB.walletBalance().map(_.balance)
    } yield {
      assert(newBalB == oldBalB + sendAmt)
      // account for variance in fees
      assert(newBalA === oldBalA - sendAmt - feeRate.calc(tx) +- Satoshis(6))

      assert(details.tx == tx)
      assert(details.txId == tx.txIdBE)
      assert(details.outputDetails.flatMap(_.addressOpt).contains(addr))
      assert(details.amount == sendAmt)
    }
  }

  it must "fund from an no input psbt" in { params =>
    val (_, lnd, _) = params

    val spk = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
    val output = TransactionOutput(Satoshis(10000), spk)
    val unsignedTx = BaseTransaction(version = Int32.one,
                                     inputs = Vector.empty,
                                     outputs = Vector(output),
                                     lockTime = UInt32.zero)

    for {
      unsignedPsbt <- lnd.fundPSBT(PSBT.fromUnsignedTx(unsignedTx),
                                   SatoshisPerVirtualByte.one,
                                   spendUnconfirmed = true)
      signed <- lnd.finalizePSBT(unsignedPsbt)
      transaction <- Future.fromTry(signed.extractTransactionAndValidate)
      errorOpt <- lnd.publishTransaction(transaction)
    } yield assert(errorOpt.isEmpty)
  }

  it must "send and receive a custom message" in { params =>
    val (_, lndA, lndB) = params

    val customMessage = UnknownTLV(BigSizeUInt(48000), hex"0094355324")

    val subscribeF = lndA.subscribeCustomMessages().runWith(Sink.head)

    for {
      nodeIdA <- lndA.nodeId
      nodeIdB <- lndB.nodeId

      _ <- lndB.sendCustomMessage(nodeIdA, customMessage)
      (nodeId, tlv) <- subscribeF
    } yield {
      assert(nodeId == nodeIdB)
      assert(tlv == customMessage)
    }
  }

  it must "probe an amount" in { params =>
    val (_, lndA, lndB) = params

    for {
      nodeIdA <- lndA.nodeId
      routes <- lndB.probe(Satoshis(1000), nodeIdA, Vector.empty)
    } yield {
      assert(routes.nonEmpty)
    }
  }

  it must "probe and pay" in { params =>
    val (_, lndA, lndB) = params

    for {
      inv <- lndA.addInvoice("test", Satoshis(1000), 3600)
      paymentOpt <- lndB.probeAndPay(inv.invoice)
    } yield {
      paymentOpt match {
        case Some(payment) =>
          assert(payment.status.isSucceeded)
        case None => fail()
      }
    }
  }

  it must "send to route with a 0 amount invoice" in { params =>
    val (_, lndA, lndB) = params

    val request = Invoice(memo = "0 amount", expiry = 3600)
    val amount = Satoshis(1000)

    for {
      inv <- lndA.addInvoice(request)
      routes <- lndB.probe(amount, inv.invoice.nodeId, Vector.empty)
      paymentOpt <- lndB.attemptToPayRoutes(inv.invoice, routes)
    } yield {
      paymentOpt match {
        case Some(payment) =>
          assert(payment.status.isSucceeded)
        case None => fail()
      }
    }
  }
}
