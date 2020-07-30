package org.bitcoins.server

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.sbclient.{Exchange, TradingPair}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
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

case class SendRawTransaction(tx: Transaction)

object SendRawTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendRawTransaction] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(SendRawTransaction(jsToTx(jsArr.arr.head)))
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

trait JsonResponse {
  def escaped: Boolean
}

case class CreateDLCOffer(
    oracleInfo: OracleInfo,
    contractInfo: ContractInfo,
    collateral: Satoshis,
    feeRateOpt: Option[SatoshisPerVirtualByte],
    locktime: UInt32,
    refundLocktime: UInt32,
    escaped: Boolean)
    extends JsonResponse

object CreateDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDLCOffer] = {

    jsArr.arr.toList match {
      case oracleInfoJs :: contractInfoJs :: collateralJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: escapedJs :: Nil =>
        Try {
          val oracleInfo = jsToOracleInfo(oracleInfoJs)
          val contractInfo = jsToContractInfo(contractInfoJs)
          val collateral = jsToSatoshis(collateralJs)
          val feeRate = jsToSatoshisPerVirtualByteOpt(feeRateOptJs)
          val locktime = jsToUInt32(locktimeJs)
          val refundLT = jsToUInt32(refundLTJs)
          val escaped = escapedJs.bool
          CreateDLCOffer(oracleInfo,
                         contractInfo,
                         collateral,
                         feeRate,
                         locktime,
                         refundLT,
                         escaped)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 7"))
    }
  }
}

case class AcceptDLCOffer(offer: DLCOffer, escaped: Boolean)
    extends JsonResponse

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

case class SignDLC(accept: DLCAccept, escaped: Boolean) extends JsonResponse

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

case class InitDLCMutualClose(
    eventId: Sha256DigestBE,
    oracleSig: SchnorrDigitalSignature,
    escaped: Boolean)
    extends JsonResponse

object InitDLCMutualClose extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[InitDLCMutualClose] = {
    jsArr.arr.toList match {
      case eventIdJs :: sigJs :: escapedJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val oracleSig = jsToSchnorrDigitalSignature(sigJs)
          val escaped = escapedJs.bool
          InitDLCMutualClose(eventId, oracleSig, escaped)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class AcceptDLCMutualClose(
    mutualCloseSig: DLCMutualCloseSig,
    noBroadcast: Boolean)
    extends Broadcastable

object AcceptDLCMutualClose extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptDLCMutualClose] = {
    jsArr.arr.toList match {
      case mutualCloseSigJs :: noBroadcastJs :: Nil =>
        Try {
          val mutualCloseSig =
            DLCMutualCloseSig.fromJson(ujson.read(mutualCloseSigJs.str))
          val noBroadcast = noBroadcastJs.bool
          AcceptDLCMutualClose(mutualCloseSig, noBroadcast)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetDLCFundingTx(eventId: Sha256DigestBE)

object GetDLCFundingTx extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetDLCFundingTx] = {
    jsArr.arr.toList match {
      case eventIdJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          GetDLCFundingTx(eventId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing eventId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class BroadcastDLCFundingTx(eventId: Sha256DigestBE)

object BroadcastDLCFundingTx extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[BroadcastDLCFundingTx] = {
    jsArr.arr.toList match {
      case eventIdJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          BroadcastDLCFundingTx(eventId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing eventId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class ExecuteDLCUnilateralClose(
    eventId: Sha256DigestBE,
    oracleSig: SchnorrDigitalSignature,
    noBroadcast: Boolean)
    extends Broadcastable

object ExecuteDLCUnilateralClose extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLCUnilateralClose] = {
    jsArr.arr.toList match {
      case eventIdJs :: oracleSigJs :: noBroadcastJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val oracleSig = jsToSchnorrDigitalSignature(oracleSigJs)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLCUnilateralClose(eventId, oracleSig, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing eventId and oracleSig arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ExecuteDLCRemoteUnilateralClose(
    eventId: Sha256DigestBE,
    cet: Transaction,
    noBroadcast: Boolean)
    extends Broadcastable

object ExecuteDLCRemoteUnilateralClose extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLCRemoteUnilateralClose] = {
    jsArr.arr.toList match {
      case eventIdJs :: cetJs :: noBroadcastJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val cet = jsToTx(cetJs)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLCRemoteUnilateralClose(eventId, cet, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing eventId and cet arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ExecuteDLCForceClose(
    eventId: Sha256DigestBE,
    oracleSig: SchnorrDigitalSignature,
    noBroadcast: Boolean)
    extends Broadcastable

object ExecuteDLCForceClose extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLCForceClose] = {
    jsArr.arr.toList match {
      case eventIdJs :: oracleSigJs :: noBroadcastJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val oracleSig = jsToSchnorrDigitalSignature(oracleSigJs)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLCForceClose(eventId, oracleSig, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing eventId and oracleSig arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ClaimDLCRemoteFunds(
    eventId: Sha256DigestBE,
    forceCloseTx: Transaction,
    noBroadcast: Boolean)
    extends Broadcastable

object ClaimDLCRemoteFunds extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ClaimDLCRemoteFunds] = {
    jsArr.arr.toList match {
      case eventIdJs :: forceCloseTxJs :: noBroadcastJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val forceCloseTx = jsToTx(forceCloseTxJs)
          val noBroadcast = noBroadcastJs.bool

          ClaimDLCRemoteFunds(eventId, forceCloseTx, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing eventId and forceCloseTx arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ExecuteDLCRefund(eventId: Sha256DigestBE, noBroadcast: Boolean)
    extends Broadcastable

object ExecuteDLCRefund extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLCRefund] = {
    jsArr.arr.toList match {
      case eventIdJs :: noBroadcastJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLCRefund(eventId, noBroadcast)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing eventId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class ClaimDLCPenaltyFunds(
    eventId: Sha256DigestBE,
    forceCloseTx: Transaction,
    noBroadcast: Boolean)
    extends Broadcastable

object ClaimDLCPenaltyFunds extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ClaimDLCPenaltyFunds] = {
    jsArr.arr.toList match {
      case eventIdJs :: forceCloseTxJs :: noBroadcastJs :: Nil =>
        Try {
          val eventId = Sha256DigestBE(eventIdJs.str)
          val forceCloseTx = jsToTx(forceCloseTxJs)
          val noBroadcast = noBroadcastJs.bool

          ClaimDLCPenaltyFunds(eventId, forceCloseTx, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing eventId and forceCloseTx arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
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

// SbClient

case class OpenSbChannel(amount: Satoshis)

object OpenSbChannel extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[OpenSbChannel] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(OpenSbChannel(jsToSatoshis(jsArr.arr.head)))
  }
}

trait PriceDataApiCall {
  def exchange: Exchange
  def tradingPair: TradingPair
}

case class GetSbPubKey(exchange: Exchange, tradingPair: TradingPair)
    extends PriceDataApiCall

object GetSbPubKey extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetSbPubKey] = {
    jsArr.arr.toList match {
      case exchangeJs :: tradingPairJs :: Nil =>
        Try {
          val exchange = Exchange.fromString(exchangeJs.str).get
          val tradingPair = TradingPair.fromString(tradingPairJs.str)

          GetSbPubKey(exchange, tradingPair)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing exchange and tradingPair arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetSbRValue(exchange: Exchange, tradingPair: TradingPair)
    extends PriceDataApiCall

object GetSbRValue extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetSbRValue] = {
    jsArr.arr.toList match {
      case exchangeJs :: tradingPairJs :: Nil =>
        Try {
          val exchange = Exchange.fromString(exchangeJs.str).get
          val tradingPair = TradingPair.fromString(tradingPairJs.str)

          GetSbRValue(exchange, tradingPair)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing exchange and tradingPair arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetSbOracleInfo(exchange: Exchange, tradingPair: TradingPair)
    extends PriceDataApiCall

object GetSbOracleInfo extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetSbOracleInfo] = {
    jsArr.arr.toList match {
      case exchangeJs :: tradingPairJs :: Nil =>
        Try {
          val exchange = Exchange.fromString(exchangeJs.str).get
          val tradingPair = TradingPair.fromString(tradingPairJs.str)

          GetSbOracleInfo(exchange, tradingPair)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing exchange and tradingPair arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetSbLastSig(exchange: Exchange, tradingPair: TradingPair)
    extends PriceDataApiCall

object GetSbLastSig extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetSbLastSig] = {
    jsArr.arr.toList match {
      case exchangeJs :: tradingPairJs :: Nil =>
        Try {
          val exchange = Exchange.fromString(exchangeJs.str).get
          val tradingPair = TradingPair.fromString(tradingPairJs.str)

          GetSbLastSig(exchange, tradingPair)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing exchange and tradingPair arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

trait ServerJsonModels {

  def jsToOracleInfo(js: Value): OracleInfo =
    js match {
      case str: Str =>
        OracleInfo(str.value)
      case _: Value =>
        throw Value.InvalidData(js, "Expected an OracleInfo as a hex string")
    }

  def jsToContractInfo(js: Value): ContractInfo =
    js match {
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

  def jsToUInt32(js: Value): UInt32 =
    js match {
      case str: Str =>
        UInt32(BigInt(str.value))
      case num: Num =>
        UInt32(num.value.toLong)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a UInt32")
    }

  def jsToSatoshis(js: Value): Satoshis =
    js match {
      case str: Str =>
        Satoshis(BigInt(str.value))
      case num: Num =>
        Satoshis(num.value.toLong)
      case _: Value =>
        throw Value.InvalidData(js, "Expected value in Satoshis")
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

  def jsToTransactionOutPointSeq(js: Value): Seq[TransactionOutPoint] = {
    js.arr.foldLeft(Seq.empty[TransactionOutPoint])((seq, outPoint) =>
      seq :+ jsToTransactionOutPoint(outPoint))
  }

  def jsToTransactionOutPoint(js: Value): TransactionOutPoint =
    TransactionOutPoint(js.str)

  def jsToCoinSelectionAlgo(js: Value): CoinSelectionAlgo =
    CoinSelectionAlgo
      .fromString(js.str)
      .getOrElse(
        throw new IllegalArgumentException("Invalid CoinSelectionAlgo"))

  def jsToTx(js: Value): Transaction = Transaction.fromHex(js.str)

  def nullToOpt(value: Value): Option[Value] =
    value match {
      case Null                      => None
      case Arr(arr) if arr.isEmpty   => None
      case Arr(arr) if arr.size == 1 => Some(arr.head)
      case _: Value                  => Some(value)
    }

  def jsToSchnorrDigitalSignature(js: Value): SchnorrDigitalSignature =
    js match {
      case str: Str =>
        SchnorrDigitalSignature(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected a SchnorrDigitalSignature as a hex string")
    }
}
