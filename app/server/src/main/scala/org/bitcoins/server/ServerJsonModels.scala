package org.bitcoins.server

import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage._
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

case class CreateDLCOffer(
    oracleInfo: OracleInfo,
    contractInfo: ContractInfo,
    feeRateOpt: Option[SatoshisPerVirtualByte],
    locktime: UInt32,
    refundLocktime: UInt32,
    escaped: Boolean)

object CreateDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDLCOffer] = {

    jsArr.arr.toList match {
      case oracleInfoJs :: contractInfoJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: escapedJs :: Nil =>
        Try {
          val oracleInfo = jsToOracleInfo(oracleInfoJs)
          val contractInfo = jsToContractInfo(contractInfoJs)
          val feeRate = jsToSatoshisPerVirtualByteOpt(feeRateOptJs)
          val locktime = jsToUInt32(locktimeJs)
          val refundLT = jsToUInt32(refundLTJs)
          val escaped = escapedJs.bool
          CreateDLCOffer(oracleInfo,
                         contractInfo,
                         feeRate,
                         locktime,
                         refundLT,
                         escaped)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 5"))
    }
  }
}

case class AcceptDLCOffer(offer: DLCOffer, escaped: Boolean)

object AcceptDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptDLCOffer] = {
    jsArr.arr.toList match {
      case offerJs :: escapedJs :: Nil =>
        Try {
          val offer = DLCOffer.fromJson(ujson.read(offerJs.str))
          val escaped = escapedJs.bool
          AcceptDLCOffer(offer, escaped)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing offer arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2, got"))
    }
  }
}

case class SignDLC(accept: DLCAccept, escaped: Boolean)

object SignDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignDLC] = {
    jsArr.arr.toList match {
      case acceptJs :: escapedJs :: Nil =>
        Try {
          val accept = DLCAccept.fromJson(ujson.read(acceptJs.str))
          val escaped = escapedJs.bool
          SignDLC(accept, escaped)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing accept and escaped arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class AddDLCSigs(sigs: DLCSign)

object AddDLCSigs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AddDLCSigs] = {
    jsArr.arr.toList match {
      case sigsJs :: Nil =>
        Try {
          val sigs = DLCSign.fromJson(ujson.read(sigsJs.str))
          AddDLCSigs(sigs)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing sigs argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

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

trait ServerJsonModels {

  def jsToOracleInfo(js: Value): OracleInfo = js match {
    case str: Str =>
      OracleInfo(str.value)
    case _: Value =>
      throw Value.InvalidData(js, "Expected an OracleInfo as a hex string")
  }

  def jsToContractInfo(js: Value): ContractInfo = js match {
    case str: Str =>
      ContractInfo(str.value)
    case _: Value =>
      throw Value.InvalidData(js, "Expected a ContractInfo as a hex string")
  }

  def jsToSatoshisPerVirtualByteOpt(js: Value): Option[SatoshisPerVirtualByte] =
    nullToOpt(js).map {
      case str: Str =>
        SatoshisPerVirtualByte(Satoshis(str.value))
      case num: Num =>
        SatoshisPerVirtualByte(Satoshis(num.value.toLong))
      case _: Value =>
        throw Value.InvalidData(js, "Expected a fee rate in sats/vbyte")
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
