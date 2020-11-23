package org.bitcoins.server

import java.io.File
import java.nio.file.Path
import java.time.Instant

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto.{
  SchnorrDigitalSignature,
  SchnorrNonce,
  Sha256DigestBE
}
import scodec.bits.ByteVector
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

case class GetDLC(paramHash: Sha256DigestBE)

object GetDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetDLC] = {
    jsArr.arr.toList match {
      case paramHashJs :: Nil =>
        Try {
          val paramHash = Sha256DigestBE(paramHashJs.str)
          GetDLC(paramHash)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing paramHash argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class CreateDLCOffer(
    oracleInfo: OracleInfo,
    contractInfoTLV: ContractInfoTLV,
    collateral: Satoshis,
    feeRateOpt: Option[SatoshisPerVirtualByte],
    locktime: UInt32,
    refundLocktime: UInt32)

object CreateDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDLCOffer] = {

    jsArr.arr.toList match {
      case oracleInfoJs :: contractInfoJs :: collateralJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: Nil =>
        Try {
          val oracleInfo = jsToOracleInfo(oracleInfoJs)
          val contractInfoTLV = jsToContractInfoTLV(contractInfoJs)
          val collateral = jsToSatoshis(collateralJs)
          val feeRate = jsToSatoshisPerVirtualByteOpt(feeRateOptJs)
          val locktime = jsToUInt32(locktimeJs)
          val refundLT = jsToUInt32(refundLTJs)
          CreateDLCOffer(oracleInfo,
                         contractInfoTLV,
                         collateral,
                         feeRate,
                         locktime,
                         refundLT)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 6"))
    }
  }
}

case class AcceptDLCOffer(offer: LnMessage[DLCOfferTLV])

object AcceptDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptDLCOffer] = {
    jsArr.arr.toList match {
      case offerJs :: Nil =>
        Try {
          val offer = LnMessageFactory(DLCOfferTLV).fromHex(offerJs.str)
          AcceptDLCOffer(offer)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing offer argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class SignDLC(accept: LnMessage[DLCAcceptTLV])

object SignDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignDLC] = {
    jsArr.arr.toList match {
      case acceptJs :: Nil =>
        Try {
          val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptJs.str)
          SignDLC(accept)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing accept argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class AddDLCSigs(sigs: LnMessage[DLCSignTLV])

object AddDLCSigs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AddDLCSigs] = {
    jsArr.arr.toList match {
      case sigsJs :: Nil =>
        Try {
          val sigs = LnMessageFactory(DLCSignTLV).fromHex(sigsJs.str)
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

case class DLCDataFromFile(path: Path)

object DLCDataFromFile extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DLCDataFromFile] = {
    jsArr.arr.toList match {
      case pathJs :: Nil =>
        Try {
          val path = new File(pathJs.str).toPath
          DLCDataFromFile(path)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing path argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetDLCFundingTx(contractId: ByteVector)

object GetDLCFundingTx extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetDLCFundingTx] = {
    jsArr.arr.toList match {
      case contractIdJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          GetDLCFundingTx(contractId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class BroadcastDLCFundingTx(contractId: ByteVector)

object BroadcastDLCFundingTx extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[BroadcastDLCFundingTx] = {
    jsArr.arr.toList match {
      case contractIdJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          BroadcastDLCFundingTx(contractId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class ExecuteDLC(
    contractId: ByteVector,
    oracleSigs: Vector[SchnorrDigitalSignature],
    noBroadcast: Boolean)
    extends Broadcastable

object ExecuteDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLC] = {
    jsArr.arr.toList match {
      case contractIdJs :: oracleSigsJs :: noBroadcastJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          val oracleSigs = jsToSchnorrDigitalSignatureVec(oracleSigsJs)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLC(contractId, oracleSigs, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing contractId, oracleSig, and noBroadcast arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ExecuteDLCRefund(contractId: ByteVector, noBroadcast: Boolean)
    extends Broadcastable

object ExecuteDLCRefund extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLCRefund] = {
    jsArr.arr.toList match {
      case contractIdJs :: noBroadcastJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLCRefund(contractId, noBroadcast)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
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

case class SignEvent(nonce: SchnorrNonce, outcome: String)

object SignEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignEvent] = {
    jsArr.arr.toList match {
      case nonceJs :: outcomeJs :: Nil =>
        Try {
          val nonce = SchnorrNonce(nonceJs.str)
          val outcome = outcomeJs.str

          SignEvent(nonce, outcome)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing nonce and outcome arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetEvent(nonce: SchnorrNonce)

object GetEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetEvent] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")
    Try {
      val nonce = SchnorrNonce(jsArr.arr.head.str)

      GetEvent(nonce)
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

  def jsToContractInfoTLV(js: Value): ContractInfoTLV =
    js match {
      case str: Str =>
        ContractInfoTLV(str.value)
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

  def jsToSchnorrDigitalSignature(js: Value): SchnorrDigitalSignature =
    js match {
      case str: Str =>
        SchnorrDigitalSignature(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected a SchnorrDigitalSignature as a hex string")
    }

  def jsToSchnorrDigitalSignatureVec(
      js: Value): Vector[SchnorrDigitalSignature] = {
    js.arr.foldLeft(Vector.empty[SchnorrDigitalSignature])((vec, sig) =>
      vec :+ jsToSchnorrDigitalSignature(sig))
  }
}
