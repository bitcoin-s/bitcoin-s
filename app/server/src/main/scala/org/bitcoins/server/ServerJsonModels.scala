package org.bitcoins.server

import org.bitcoins.appCommons.JsonSerializers
import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.ptlc.PTLCMessage._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import ujson._
import upickle.default._

import scala.util.{Failure, Try}

// TODO ID?
case class ServerCommand(method: String, params: ujson.Arr)

object ServerCommand {
  implicit val rw: ReadWriter[ServerCommand] = macroRW
}

case class GetBalance(isSats: Boolean)

object GetBalance extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetBalance] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetBalance(jsArr.arr.head.bool))
  }
}

case class GetConfirmedBalance(isSats: Boolean)

object GetConfirmedBalance extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetConfirmedBalance] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetConfirmedBalance(jsArr.arr.head.bool))
  }
}

case class GetUnconfirmedBalance(isSats: Boolean)

object GetUnconfirmedBalance extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetUnconfirmedBalance] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetUnconfirmedBalance(jsArr.arr.head.bool))
  }
}

case class GetAddressInfo(address: BitcoinAddress)

object GetAddressInfo extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAddressInfo] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    val address = jsToBitcoinAddress(jsArr.arr.head)

    Try(GetAddressInfo(address))
  }
}

case class CombinePSBTs(psbts: Seq[PSBT])

object CombinePSBTs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CombinePSBTs] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(CombinePSBTs(jsToPSBTSeq(jsArr.arr.head)))
  }
}

case class JoinPSBTs(psbts: Seq[PSBT])

object JoinPSBTs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[JoinPSBTs] = {
    CombinePSBTs
      .fromJsArr(jsArr)
      .map(combine => JoinPSBTs(combine.psbts))
  }
}

case class FinalizePSBT(psbt: PSBT)

object FinalizePSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[FinalizePSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(FinalizePSBT(jsToPSBT(jsArr.arr.head)))
  }
}

case class ExtractFromPSBT(psbt: PSBT)

object ExtractFromPSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExtractFromPSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(ExtractFromPSBT(jsToPSBT(jsArr.arr.head)))
  }
}

case class ConvertToPSBT(tx: Transaction)

object ConvertToPSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ConvertToPSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(ConvertToPSBT(jsToTx(jsArr.arr.head)))
  }
}

case class Rescan(
    batchSize: Option[Int],
    startBlock: Option[BlockStamp],
    endBlock: Option[BlockStamp],
    force: Boolean)

object Rescan extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[Rescan] = {

    def parseBlockStamp(value: Value): Option[BlockStamp] =
      nullToOpt(value).map {
        case Str(value) => BlockStamp.fromString(value).get
        case Num(value) =>
          val int = value.toInt
          if (int >= 0 && int <= Int.MaxValue)
            BlockHeight(int)
          else throw Value.InvalidData(value, "Expected a positive integer")
        case _: Value =>
          throw Value.InvalidData(value, "Expected a Num or a Str")
      }

    def parseInt(value: Value): Option[Int] =
      nullToOpt(value).map {
        case Str(value) => value.toInt
        case Num(value) => value.toInt
        case _: Value =>
          throw Value.InvalidData(value, "Expected a Num or a Str")
      }

    def parseBoolean(value: Value): Boolean = value match {
      case Bool(bool) => bool
      case _: Value   => throw Value.InvalidData(value, "Expected a Bool")
    }

    jsArr.arr.toList match {
      case batchSizeJs :: startJs :: endJs :: forceJs :: Nil =>
        Try {
          val batchSize = parseInt(batchSizeJs)
          val start = parseBlockStamp(startJs)
          val end = parseBlockStamp(endJs)
          val force = parseBoolean(forceJs)
          Rescan(batchSize = batchSize,
                 startBlock = start,
                 endBlock = end,
                 force = force)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing addresses"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }

}

case class SendToAddress(
    address: BitcoinAddress,
    amount: Bitcoins,
    satoshisPerVirtualByte: Option[SatoshisPerVirtualByte])

object SendToAddress extends ServerJsonModels {

  /// TODO do this in a more coherent fashion
  // custom akka-http directive?
  def fromJsArr(jsArr: ujson.Arr): Try[SendToAddress] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: satsPerVBytesJs :: Nil =>
        Try {
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          SendToAddress(address, bitcoins, satoshisPerVirtualByte)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing address, amount, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }

}

trait JsonResponse {
  def escaped: Boolean
}

case class CreatePTLC(amount: Satoshis, timeout: UInt32, escaped: Boolean)
    extends JsonResponse

object CreatePTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreatePTLC] = {
    jsArr.arr.toList match {
      case amountJs :: timeoutJs :: escapedJs :: Nil =>
        Try {
          val amount = jsToSatoshis(amountJs)
          val timeout = jsToUInt32(timeoutJs)
          val escaped = escapedJs.bool

          CreatePTLC(amount, timeout, escaped)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing amount, timeout, and escaped arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class AcceptPTLC(
    ptlcInvoice: PTLCInvoice,
    feeRateOpt: Option[SatoshisPerVirtualByte],
    escaped: Boolean)
    extends JsonResponse

object AcceptPTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptPTLC] = {
    jsArr.arr.toList match {
      case ptlcInvoiceJs :: satsPerVBytesJs :: escapedJs :: Nil =>
        Try {
          val ptlcInvoice =
            JsonSerializers.getPTLCInvoice(ujson.read(ptlcInvoiceJs.str))
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          val escaped = escapedJs.bool

          AcceptPTLC(ptlcInvoice, satoshisPerVirtualByte, escaped)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing ptlcInvoice and escaped argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SignPTLC(ptlcAccept: PTLCAccept, escaped: Boolean)
    extends JsonResponse

object SignPTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignPTLC] = {
    jsArr.arr.toList match {
      case ptlcAcceptJs :: escapedJs :: Nil =>
        Try {
          val ptlcAccept =
            JsonSerializers.getPTLCAccept(ujson.read(ptlcAcceptJs.str))
          val escaped = escapedJs.bool

          SignPTLC(ptlcAccept, escaped)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing ptlcAccept and escaped argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class AddPTLCSig(ptlcRefundSig: PTLCRefundSignature)

object AddPTLCSig extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AddPTLCSig] = {
    jsArr.arr.toList match {
      case ptlcRefundSigJs :: Nil =>
        Try {
          val ptlcRefundSig = JsonSerializers.getPTLCRefundSignature(
            ujson.read(ptlcRefundSigJs.str))

          AddPTLCSig(ptlcRefundSig)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing ptlcRefundSig argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class BroadcastPTLC(invoiceId: Sha256DigestBE)

object BroadcastPTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[BroadcastPTLC] = {
    jsArr.arr.toList match {
      case invoiceIdJs :: Nil =>
        Try {
          val invoiceId = Sha256DigestBE(invoiceIdJs.str)
          BroadcastPTLC(invoiceId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing invoiceId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetPTLC(invoiceId: Sha256DigestBE)

object GetPTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetPTLC] = {
    jsArr.arr.toList match {
      case invoiceIdJs :: Nil =>
        Try {
          val invoiceId = Sha256DigestBE(invoiceIdJs.str)
          GetPTLC(invoiceId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing invoiceId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class ClaimPTLC(invoiceId: Sha256DigestBE)

object ClaimPTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ClaimPTLC] = {
    jsArr.arr.toList match {
      case invoiceIdJs :: Nil =>
        Try {
          val invoiceId = Sha256DigestBE(invoiceIdJs.str)
          ClaimPTLC(invoiceId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing invoiceId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetPTLCSecret(invoiceId: Sha256DigestBE, ptlcSpendTx: Transaction)

object GetPTLCSecret extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetPTLCSecret] = {
    jsArr.arr.toList match {
      case invoiceIdJs :: ptlcSpendTxJs :: Nil =>
        Try {
          val invoiceId = Sha256DigestBE(invoiceIdJs.str)
          val ptlcSpendTx = jsToTx(ptlcSpendTxJs)

          GetPTLCSecret(invoiceId, ptlcSpendTx)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing invoiceId and ptlcSpendTx argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class RefundPTLC(invoiceId: Sha256DigestBE)

object RefundPTLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[RefundPTLC] = {
    jsArr.arr.toList match {
      case invoiceIdJs :: Nil =>
        Try {
          val invoiceId = Sha256DigestBE(invoiceIdJs.str)
          RefundPTLC(invoiceId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing invoiceId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

trait ServerJsonModels {

  def jsToSatoshis(js: Value): Satoshis = js match {
    case str: Str =>
      Satoshis(BigInt(str.value))
    case num: Num =>
      Satoshis(num.value.toLong)
    case _: Value =>
      throw Value.InvalidData(js, "Expected value in Satoshis")
  }

  def jsToUInt32(js: Value): UInt32 = js match {
    case str: Str =>
      UInt32(BigInt(str.value))
    case num: Num =>
      UInt32(num.value.toLong)
    case _: Value =>
      throw Value.InvalidData(js, "Expected a UInt32")
  }

  def jsToBitcoinAddress(js: Value): BitcoinAddress = {
    try {
      BitcoinAddress.fromStringExn(js.str)
    } catch {
      case _: IllegalArgumentException =>
        throw Value.InvalidData(js, "Expected a valid address")
    }
  }

  def jsToPSBTSeq(js: Value): Seq[PSBT] = {
    js.arr.foldLeft(Seq.empty[PSBT])((seq, psbt) => seq :+ jsToPSBT(psbt))
  }

  def jsToPSBT(js: Value): PSBT = PSBT.fromString(js.str)

  def jsToTx(js: Value): Transaction = Transaction.fromHex(js.str)

  def nullToOpt(value: Value): Option[Value] = value match {
    case Null                      => None
    case Arr(arr) if arr.isEmpty   => None
    case Arr(arr) if arr.size == 1 => Some(arr.head)
    case _: Value                  => Some(value)
  }
}
