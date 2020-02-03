package org.bitcoins.server

import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage.{DLCOffer, OracleInfo}
import ujson._
import upickle.default._

import scala.util.{Failure, Try}

// TODO ID?
case class ServerCommand(method: String, params: ujson.Arr)

object ServerCommand {
  implicit val rw: ReadWriter[ServerCommand] = macroRW
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

case class CreateDLCOffer(
    amount: Bitcoins,
    oracleInfo: OracleInfo,
    contractInfo: Seq[Sha256DigestBE],
    feeRateOpt: Option[SatoshisPerVirtualByte],
    locktime: UInt32,
    refundLocktime: UInt32,
    escaped: Boolean)

object CreateDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDLCOffer] = {

    jsArr.arr.toList match {
      case amountJs :: oracleInfoJs :: contractInfoJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: escapedJs :: Nil =>
        Try {
          val amount = jsToBitcoins(amountJs)
          val oracleInfo = jsToOracleInfo(oracleInfoJs)
          val contractInfo = jsToSHA256BESeq(contractInfoJs)
          val feeRate = jsToSatoshisPerVirtualByteOpt(feeRateOptJs)
          val locktime = jsToUInt32(locktimeJs)
          val refundLT = jsToUInt32(refundLTJs)
          val escaped = escapedJs.bool
          CreateDLCOffer(amount,
                         oracleInfo,
                         contractInfo,
                         feeRate,
                         locktime,
                         refundLT,
                         escaped)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 6"))
    }
  }
}

case class AcceptDLCOffer(offer: DLCOffer, amount: Bitcoins, escaped: Boolean)

object AcceptDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptDLCOffer] = {
    jsArr.arr.toList match {
      case offerJs :: bitcoinsJs :: escapedJs :: Nil =>
        Try {
          val offer = DLCOffer.fromJson(ujson.read(offerJs.str))
          val amount = jsToBitcoins(bitcoinsJs)
          val escaped = escapedJs.bool
          AcceptDLCOffer(offer, amount, escaped)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing offer and amount arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class SendToAddress(address: BitcoinAddress, amount: Bitcoins)

object SendToAddress extends ServerJsonModels {

  /// TODO do this in a more coherent fashion
  // custom akka-http directive?
  def fromJsArr(jsArr: ujson.Arr): Try[SendToAddress] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: Nil =>
        Try {
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          SendToAddress(address, bitcoins)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing address and amount argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }

}

trait ServerJsonModels {

  def nullToOpt(value: Value): Option[Value] = value match {
    case Null                      => None
    case Arr(arr) if arr.isEmpty   => None
    case Arr(arr) if arr.size == 1 => Some(arr.head)
    case _: Value                  => Some(value)
  }

  def jsToSHA256BESeq(js: Value): Seq[Sha256DigestBE] = {
    js.arr.value.foldLeft(Seq.empty[Sha256DigestBE])((seq, value) =>
      value match {
        case str: Str =>
          seq :+ Sha256DigestBE(str.value)
        case _: Value =>
          throw Value.InvalidData(js, "Expected a list of SHA256Digests")
      })
  }

  def jsToOracleInfo(js: Value): OracleInfo = js match {
    case str: Str =>
      OracleInfo(str.value)
    case _: Value =>
      throw Value.InvalidData(js, "Expected an OracleInfo as a hex string")
  }

  def jsToBitcoins(js: Value): Bitcoins = js match {
    case str: Str =>
      Bitcoins(BigDecimal(str.value))
    case num: Num =>
      Bitcoins(BigDecimal(num.value))
    case _: Value =>
      throw Value.InvalidData(js, "Expected a valid bitcoin amount")
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

}
