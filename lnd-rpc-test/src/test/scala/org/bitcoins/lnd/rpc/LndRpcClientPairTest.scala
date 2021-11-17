package org.bitcoins.lnd.rpc

import akka.stream.scaladsl.Sink
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.{currencyUnitNumeric, Bitcoins, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.tlv.UnknownTLV
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.crypto._
import org.bitcoins.testkit.fixtures.DualLndFixture
import scodec.bits.HexStringSyntax

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
      assert(infoA.blockHeight >= 0)
      assert(infoB.blockHeight >= 0)
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
      addr <- lnd.getNewAddress
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
      assert(details.destAddresses.contains(addr))
      assert(details.amount == sendAmt)
    }
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
}
