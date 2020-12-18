package org.bitcoins.server

import java.time.Instant
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto.{AesPassword, DoubleSha256DigestBE}
import ujson._
import upickle.default._

import scala.util.{Failure, Try}

// TODO ID?
case class ServerCommand(method: String, params: ujson.Arr)

object ServerCommand {
  implicit val rw: ReadWriter[ServerCommand] = macroRW
}

case class GetNewAddress(labelOpt: Option[AddressLabelTag])

object GetNewAddress extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetNewAddress] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    val labelOpt = nullToOpt(jsArr.arr.head).map {
      case Str(str) =>
        AddressLabelTag(str)
      case value: Value =>
        throw Value.InvalidData(value, "Expected a String")
    }

    Try(GetNewAddress(labelOpt))
  }
}

case class LockUnspent(
    unlock: Boolean,
    outputParam: Vector[LockUnspentOutputParameter])

object LockUnspent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[LockUnspent] = {
    jsArr.arr.toList match {
      case unlockJs :: outPointsJs :: Nil =>
        Try {
          val unlock = unlockJs.bool
          val outPoints = jsToLockUnspentOutputParameters(outPointsJs).toVector

          LockUnspent(unlock, outPoints)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class LabelAddress(address: BitcoinAddress, label: AddressLabelTag)

object LabelAddress extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[LabelAddress] = {
    jsArr.arr.toList match {
      case addrJs :: labelJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)
          val label = AddressLabelTag(labelJs.str)

          LabelAddress(addr, label)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetAddressTags(address: BitcoinAddress)

object GetAddressTags extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAddressTags] = {
    jsArr.arr.toList match {
      case addrJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)

          GetAddressTags(addr)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetAddressLabels(address: BitcoinAddress)

object GetAddressLabels extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAddressLabels] = {
    jsArr.arr.toList match {
      case addrJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)

          GetAddressLabels(addr)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DropAddressLabels(address: BitcoinAddress)

object DropAddressLabels extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DropAddressLabels] = {
    jsArr.arr.toList match {
      case addrJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)

          DropAddressLabels(addr)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
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

case class SendRawTransaction(tx: Transaction)

object SendRawTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendRawTransaction] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(SendRawTransaction(jsToTx(jsArr.arr.head)))
  }
}

case class KeyManagerPassphraseChange(
    oldPassword: AesPassword,
    newPassword: AesPassword)

object KeyManagerPassphraseChange extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[KeyManagerPassphraseChange] = {
    jsArr.arr.toList match {
      case oldPassJs :: newPassJs :: Nil =>
        Try {
          val oldPass = AesPassword.fromString(oldPassJs.str)
          val newPass = AesPassword.fromString(newPassJs.str)

          KeyManagerPassphraseChange(oldPass, newPass)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing old password and new password arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class KeyManagerPassphraseSet(password: AesPassword)

object KeyManagerPassphraseSet extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[KeyManagerPassphraseSet] = {
    jsArr.arr.toList match {
      case passJs :: Nil =>
        Try {
          val pass = AesPassword.fromString(passJs.str)

          KeyManagerPassphraseSet(pass)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing password argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
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

case class DecodeRawTransaction(tx: Transaction)

object DecodeRawTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeRawTransaction] = {
    jsArr.arr.toList match {
      case tx :: Nil =>
        Try {
          DecodeRawTransaction(Transaction.fromHex(tx.str))
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DecodePSBT(psbt: PSBT)

object DecodePSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodePSBT] = {
    jsArr.arr.toList match {
      case psbtJs :: Nil =>
        Try {
          val psbt = jsToPSBT(psbtJs)
          DecodePSBT(psbt)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class AnalyzePSBT(psbt: PSBT)

object AnalyzePSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AnalyzePSBT] = {
    jsArr.arr.toList match {
      case psbtJs :: Nil =>
        Try {
          AnalyzePSBT(jsToPSBT(psbtJs))
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class Rescan(
    batchSize: Option[Int],
    startBlock: Option[BlockStamp],
    endBlock: Option[BlockStamp],
    force: Boolean,
    ignoreCreationTime: Boolean)

object Rescan extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[Rescan] = {

    def parseBlockStamp(value: Value): Option[BlockStamp] =
      nullToOpt(value).map {
        case Str(value) => BlockStamp.fromString(value)
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

    def parseBoolean(value: Value): Boolean =
      value match {
        case Bool(bool) => bool
        case _: Value   => throw Value.InvalidData(value, "Expected a Bool")
      }

    jsArr.arr.toList match {
      case batchSizeJs :: startJs :: endJs :: forceJs :: ignoreCreationTimeJs :: Nil =>
        Try {
          val batchSize = parseInt(batchSizeJs)
          val start = parseBlockStamp(startJs)
          val end = parseBlockStamp(endJs)
          val force = parseBoolean(forceJs)
          val ignoreCreationTime = parseBoolean(ignoreCreationTimeJs)
          Rescan(batchSize = batchSize,
                 startBlock = start,
                 endBlock = end,
                 force = force,
                 ignoreCreationTime = ignoreCreationTime)
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

case class GetTransaction(txId: DoubleSha256DigestBE)

object GetTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetTransaction] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetTransaction(DoubleSha256DigestBE(jsArr.arr.head.str)))
  }
}

trait Broadcastable {
  def noBroadcast: Boolean
}

case class SendToAddress(
    address: BitcoinAddress,
    amount: Bitcoins,
    satoshisPerVirtualByte: Option[SatoshisPerVirtualByte],
    noBroadcast: Boolean)
    extends Broadcastable

object SendToAddress extends ServerJsonModels {

  /// TODO do this in a more coherent fashion
  // custom akka-http directive?
  def fromJsArr(jsArr: ujson.Arr): Try[SendToAddress] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: satsPerVBytesJs :: noBroadcastJs :: Nil =>
        Try {
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          val noBroadcast = noBroadcastJs.bool
          SendToAddress(address, bitcoins, satoshisPerVirtualByte, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing address, amount, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }

}

case class SendFromOutpoints(
    outPoints: Vector[TransactionOutPoint],
    address: BitcoinAddress,
    amount: Bitcoins,
    satoshisPerVirtualByte: Option[SatoshisPerVirtualByte])

object SendFromOutpoints extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendFromOutpoints] = {
    jsArr.arr.toList match {
      case outPointsJs :: addrJs :: bitcoinsJs :: satsPerVBytesJs :: Nil =>
        Try {
          val outPoints = jsToTransactionOutPointSeq(outPointsJs).toVector
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          SendFromOutpoints(outPoints,
                            address,
                            bitcoins,
                            satoshisPerVirtualByte)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing outPoints, address, amount, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }

}

case class SendWithAlgo(
    address: BitcoinAddress,
    amount: Bitcoins,
    satoshisPerVirtualByte: Option[SatoshisPerVirtualByte],
    algo: CoinSelectionAlgo)

object SendWithAlgo extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendWithAlgo] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: satsPerVBytesJs :: algoJs :: Nil =>
        Try {
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          val algo = jsToCoinSelectionAlgo(algoJs)

          SendWithAlgo(address, bitcoins, satoshisPerVirtualByte, algo)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing address, amount, fee rate, and algo arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }

}

case class SignPSBT(psbt: PSBT)

object SignPSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignPSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(SignPSBT(jsToPSBT(jsArr.arr.head)))
  }
}

case class OpReturnCommit(
    message: String,
    hashMessage: Boolean,
    feeRateOpt: Option[SatoshisPerVirtualByte])

object OpReturnCommit extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[OpReturnCommit] = {
    jsArr.arr.toList match {
      case messageJs :: hashMessageJs :: feeRateOptJs :: Nil =>
        Try {
          val message = messageJs.str
          val hashMessage = hashMessageJs.bool
          val feeRateOpt =
            nullToOpt(feeRateOptJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          OpReturnCommit(message, hashMessage, feeRateOpt)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing message, hashMessage, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

// Oracle Models

case class CreateEvent(
    label: String,
    maturationTime: Instant,
    outcomes: Vector[String])

object CreateEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateEvent] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: outcomesJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime: Instant =
            Instant.ofEpochSecond(maturationTimeJs.num.toLong)
          val outcomes = outcomesJs.arr.map(_.str).toVector

          CreateEvent(label, maturationTime, outcomes)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing label and outcome arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class CreateRangedEvent(
    eventName: String,
    maturationTime: Instant,
    start: Int,
    stop: Int,
    step: Int,
    unit: String,
    precision: Int)

object CreateRangedEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateRangedEvent] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: startJs :: stopJs :: stepJs :: unitJs :: precisionJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime: Instant =
            Instant.ofEpochSecond(maturationTimeJs.num.toLong)
          val start = startJs.num.toInt
          val stop = stopJs.num.toInt
          val step = stepJs.num.toInt
          val unit = unitJs.str
          val precision = precisionJs.num.toInt

          CreateRangedEvent(label,
                            maturationTime,
                            start,
                            stop,
                            step,
                            unit,
                            precision)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing label, maturationTime, start, stop, and step arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 5"))
    }
  }
}

case class CreateDigitDecompEvent(
    eventName: String,
    maturationTime: Instant,
    base: Int,
    isSigned: Boolean,
    numDigits: Int,
    unit: String,
    precision: Int)

object CreateDigitDecompEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDigitDecompEvent] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: baseJs :: isSignedJs :: numDigitsJs :: unitJs :: precisionJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime: Instant =
            Instant.ofEpochSecond(maturationTimeJs.num.toLong)
          val base = baseJs.num.toInt
          val isSigned = isSignedJs.bool
          val numDigits = numDigitsJs.num.toInt
          val unit = unitJs.str
          val precision = precisionJs.num.toInt

          CreateDigitDecompEvent(label,
                                 maturationTime,
                                 base,
                                 isSigned,
                                 numDigits,
                                 unit,
                                 precision)
        }
      case Nil =>
        Failure(new IllegalArgumentException(
          "Missing label, maturationTime, base, isSigned, and numDigits arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 5"))
    }
  }
}

case class SignEvent(oracleEventTLV: OracleEventV0TLV, outcome: String)

object SignEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignEvent] = {
    jsArr.arr.toList match {
      case tlvJs :: outcomeJs :: Nil =>
        Try {
          val oracleEventTLV = OracleEventV0TLV(tlvJs.str)
          val outcome = outcomeJs.str

          SignEvent(oracleEventTLV, outcome)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing oracle event tlv and outcome arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SignForRange(oracleEventTLV: OracleEventV0TLV, num: Long)

object SignForRange extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignForRange] = {
    jsArr.arr.toList match {
      case tlvJs :: numJs :: Nil =>
        Try {
          val oracleEventTLV = OracleEventV0TLV(tlvJs.str)
          val num = numJs match {
            case num: Num => num.value
            case str: Str => str.value.toDouble
            case _: Value =>
              throw new IllegalArgumentException(
                s"Unable to parse $numJs as a number")
          }

          SignForRange(oracleEventTLV, num.toLong)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing oracle event tlv and num arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SignDigits(oracleEventTLV: OracleEventV0TLV, num: Long)

object SignDigits extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignDigits] = {
    jsArr.arr.toList match {
      case tlvJs :: numJs :: Nil =>
        Try {
          val oracleEventTLV = OracleEventV0TLV(tlvJs.str)
          val num = numJs match {
            case num: Num => num.value
            case str: Str => str.value.toDouble
            case _: Value =>
              throw new IllegalArgumentException(
                s"Unable to parse $numJs as a number")
          }

          SignDigits(oracleEventTLV, num.toLong)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing oracle event tlv and num arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetEvent(oracleEventTLV: OracleEventV0TLV)

object GetEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetEvent] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")
    Try {
      val oracleEventTLV = OracleEventV0TLV(jsArr.arr.head.str)

      GetEvent(oracleEventTLV)
    }
  }
}

trait ServerJsonModels {

  def jsToBitcoinAddress(js: Value): BitcoinAddress = {
    try {
      BitcoinAddress.fromString(js.str)
    } catch {
      case _: IllegalArgumentException =>
        throw Value.InvalidData(js, "Expected a valid address")
    }
  }

  def jsToPSBTSeq(js: Value): Seq[PSBT] = {
    js.arr.foldLeft(Seq.empty[PSBT])((seq, psbt) => seq :+ jsToPSBT(psbt))
  }

  def jsToPSBT(js: Value): PSBT = PSBT.fromString(js.str)

  def jsToTransactionOutPointSeq(js: Value): Seq[TransactionOutPoint] = {
    js.arr.foldLeft(Seq.empty[TransactionOutPoint])((seq, outPoint) =>
      seq :+ jsToTransactionOutPoint(outPoint))
  }

  def jsToTransactionOutPoint(js: Value): TransactionOutPoint =
    TransactionOutPoint(js.str)

  def jsToLockUnspentOutputParameter(js: Value): LockUnspentOutputParameter =
    LockUnspentOutputParameter.fromJson(js)

  def jsToLockUnspentOutputParameters(
      js: Value): Seq[LockUnspentOutputParameter] = {
    js.arr.foldLeft(Seq.empty[LockUnspentOutputParameter])((seq, outPoint) =>
      seq :+ jsToLockUnspentOutputParameter(outPoint))
  }

  def jsToCoinSelectionAlgo(js: Value): CoinSelectionAlgo =
    CoinSelectionAlgo
      .fromString(js.str)

  def jsToTx(js: Value): Transaction = Transaction.fromHex(js.str)

  def nullToOpt(value: Value): Option[Value] =
    value match {
      case Null                      => None
      case Arr(arr) if arr.isEmpty   => None
      case Arr(arr) if arr.size == 1 => Some(arr.head)
      case _: Value                  => Some(value)
    }
}
