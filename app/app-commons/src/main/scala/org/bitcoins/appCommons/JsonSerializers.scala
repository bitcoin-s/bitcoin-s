package org.bitcoins.appCommons

import org.bitcoins.core.crypto.{
  ECAdaptorSignature,
  ECPublicKey,
  Sha256DigestBE
}
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ptlc.PTLCMessage
import org.bitcoins.core.protocol.ptlc.PTLCMessage._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import ujson._

import scala.collection.mutable

object JsonSerializers {

  def toJson(ptlcMessage: PTLCMessage): Value = {
    ptlcMessage match {
      case invoice: PTLCInvoice =>
        toJson(invoice)
      case accept: PTLCAccept =>
        toJson(accept)
      case refundSig: PTLCRefundSignature =>
        toJson(refundSig)
    }
  }

  def toJson(invoice: PTLCInvoice): Value = {
    Obj(
      mutable.LinkedHashMap[String, Value](
        "adaptorPoint" -> Str(invoice.adaptorPoint.hex),
        "amount" -> Num(invoice.amount.satoshis.toLong),
        "pubkey" -> Str(invoice.pubkey.hex),
        "finalAddress" -> Str(invoice.finalAddress.value),
        "timeout" -> Num(invoice.timeout.toLong)
      )
    )
  }

  def toJson(accept: PTLCAccept): Value = {
    Obj(
      mutable.LinkedHashMap[String, Value](
        "pubkey" -> Str(accept.pubkey.hex),
        "unsignedTx" -> Str(accept.unsignedTx.hex),
        "adaptorSignature" -> Str(accept.adaptorSignature.hex),
        "refundAddress" -> Str(accept.refundAddress.value),
        "feeRate" -> Num(accept.feeRate.currencyUnit.satoshis.toLong),
        "invoiceId" -> Str(accept.invoiceId.hex)
      )
    )
  }

  def toJson(refundSig: PTLCRefundSignature): Value = {
    Obj(
      mutable.LinkedHashMap[String, Value](
        "refundSignature" -> Str(refundSig.refundSignature.hex),
        "invoiceId" -> Str(refundSig.invoiceId.hex)
      )
    )
  }

  def getPTLCInvoice(js: Value): PTLCInvoice = {
    val vec = js.obj.toVector

    val adaptorPoint =
      vec
        .find(_._1 == "adaptorPoint")
        .map(obj => ECPublicKey(obj._2.str))
        .get

    val amount = vec
      .find(_._1 == "amount")
      .map(obj => Satoshis(obj._2.num.toLong))
      .get

    val pubkey =
      vec
        .find(_._1 == "pubkey")
        .map(obj => ECPublicKey(obj._2.str))
        .get

    val finalAddress =
      vec
        .find(_._1 == "finalAddress")
        .map(obj => BitcoinAddress.fromString(obj._2.str).get)
        .get

    val timeout =
      vec
        .find(_._1 == "timeout")
        .map(obj => UInt32(obj._2.num.toLong))
        .get

    PTLCInvoice(adaptorPoint = adaptorPoint,
                amount = amount,
                pubkey = pubkey,
                finalAddress = finalAddress,
                timeout = timeout)
  }

  def getPTLCAccept(js: Value): PTLCAccept = {
    val vec = js.obj.toVector

    val pubkey =
      vec
        .find(_._1 == "pubkey")
        .map(obj => ECPublicKey(obj._2.str))
        .get

    val unsignedTx =
      vec
        .find(_._1 == "unsignedTx")
        .map(obj => Transaction(obj._2.str))
        .get

    val adaptorSignature = vec
      .find(_._1 == "adaptorSignature")
      .map(obj => ECAdaptorSignature(obj._2.str))
      .get

    val refundAddress =
      vec
        .find(_._1 == "refundAddress")
        .map(obj => BitcoinAddress.fromString(obj._2.str).get)
        .get

    val feeRate =
      vec
        .find(_._1 == "feeRate")
        .map(obj => SatoshisPerVirtualByte(Satoshis(obj._2.num.toLong)))
        .get

    val invoiceId =
      vec
        .find(_._1 == "invoiceId")
        .map(obj => Sha256DigestBE(obj._2.str))
        .get

    PTLCAccept(pubkey = pubkey,
               unsignedTx = unsignedTx,
               adaptorSignature = adaptorSignature,
               refundAddress = refundAddress,
               feeRate = feeRate,
               invoiceId = invoiceId)
  }

  def getPTLCRefundSignature(js: Value): PTLCRefundSignature = {
    val vec = js.obj.toVector

    val refundSignature = vec
      .find(_._1 == "refundSignature")
      .map(obj => PartialSignature(obj._2.str))
      .get

    val invoiceId =
      vec
        .find(_._1 == "invoiceId")
        .map(obj => Sha256DigestBE(obj._2.str))
        .get

    PTLCRefundSignature(refundSignature = refundSignature,
                        invoiceId = invoiceId)
  }
}
