package org.bitcoins.dlc

import org.bitcoins.core.crypto.{
  ECPublicKey,
  SchnorrDigitalSignature,
  Sha256DigestBE
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{
  Bech32Address,
  BitcoinAddress,
  NetworkElement
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.{CryptoUtil, Factory}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import scodec.bits.ByteVector
import ujson._

import scala.annotation.tailrec
import scala.collection.{mutable, GenTraversableOnce}

sealed trait DLCMessage {
  def toJson: Value
  def toJsonStr: String = toJson.toString()
}

object DLCMessage {

  // TODO: will need to be changed when this is standardized
  def calcEventId(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts): Sha256DigestBE = {
    CryptoUtil
      .sha256(oracleInfo.bytes ++ contractInfo.bytes ++ timeouts.bytes)
      .flip
  }

  private def getValue(key: String)(
      implicit obj: mutable.LinkedHashMap[String, Value]): Value = {
    val index = obj.keys.toList.indexOf(key)
    obj.values(index)
  }

  case class OracleInfo(pubKey: ECPublicKey, rValue: ECPublicKey)
      extends NetworkElement {

    override def bytes: ByteVector = pubKey.bytes ++ rValue.bytes
  }

  object OracleInfo extends Factory[OracleInfo] {

    val empty: OracleInfo = OracleInfo(ByteVector.low(66))

    override def fromBytes(bytes: ByteVector): OracleInfo = {
      require(bytes.size == 66, s"OracleInfo is only 66 bytes, got $bytes")

      val pubkey = ECPublicKey(bytes.take(33))
      val rValue = ECPublicKey(bytes.drop(33))

      // TODO: validate rValue once the libsecp functionality is added

      OracleInfo(pubkey, rValue)
    }
  }

  case class ContractInfo(outcomeValueMap: Map[Sha256DigestBE, Satoshis])
      extends NetworkElement {
    override def bytes: ByteVector = {
      outcomeValueMap.foldLeft(ByteVector.empty) {
        case (vec, (digest, sats)) => vec ++ digest.bytes ++ sats.bytes
      }
    }
    // Give common Map methods so that users don't have to reach in every time

    def head: (Sha256DigestBE, Satoshis) = outcomeValueMap.head
    def last: (Sha256DigestBE, Satoshis) = outcomeValueMap.last

    def tail: Map[Sha256DigestBE, Satoshis] = outcomeValueMap.tail
    def init: Map[Sha256DigestBE, Satoshis] = outcomeValueMap.init

    def keys: Iterable[Sha256DigestBE] = outcomeValueMap.keys
    def values: Iterable[Satoshis] = outcomeValueMap.values

    def toVector: Vector[(Sha256DigestBE, Satoshis)] = outcomeValueMap.toVector

    def foldLeft[B](z: B)(op: (B, (Sha256DigestBE, Satoshis)) => B): B =
      outcomeValueMap.foldLeft(z)(op)

    def map[B](f: ((Sha256DigestBE, Satoshis)) => B): Iterable[B] =
      outcomeValueMap.map(f)

    def flatMap[B](
        f: ((Sha256DigestBE, Satoshis)) => GenTraversableOnce[B]): Iterable[B] =
      outcomeValueMap.flatMap(f)
  }

  object ContractInfo extends Factory[ContractInfo] {

    private val sizeOfMapElement: Int = 40

    val empty: ContractInfo = ContractInfo(ByteVector.low(40))

    override def fromBytes(bytes: ByteVector): ContractInfo = {
      @tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[(Sha256DigestBE, Satoshis)]): Vector[
        (Sha256DigestBE, Satoshis)] = {
        if (remainingBytes.size < sizeOfMapElement) {
          accum
        } else {
          val relevantBytes = remainingBytes.take(sizeOfMapElement)
          val digest = Sha256DigestBE(relevantBytes.take(32))
          val sats = Satoshis(relevantBytes.takeRight(8))
          loop(remainingBytes.drop(sizeOfMapElement), accum :+ (digest, sats))
        }
      }
      ContractInfo(loop(bytes, Vector.empty).toMap)
    }
  }

  sealed trait DLCSetupMessage extends DLCMessage {
    def pubKeys: DLCPublicKeys
    def totalCollateral: Satoshis
    def fundingInputs: Vector[OutputReference]
    def changeAddress: Bech32Address
  }

  /**
    * The initiating party starts the protocol by sending an offer message to the other party.
    *
    * @param contractInfo Contract information consists of a map to be used to create CETs
    * @param oracleInfo The oracle public key and R point(s) to use to build the CETs as
    *                   well as meta information to identify the oracle to be used in the contract.
    * @param pubKeys The relevant public keys that the initiator will be using
    * @param totalCollateral How much the initiator inputs into the contract.
    * @param fundingInputs The set of UTXOs to be used as input to the fund transaction.
    * @param changeAddress The address to use to send the change for the initiator.
    * @param feeRate The fee rate to be used when computing fees for the different transactions.
    * @param timeouts The set of timeouts for the CETs
    */
  case class DLCOffer(
      contractInfo: ContractInfo,
      oracleInfo: OracleInfo,
      pubKeys: DLCPublicKeys,
      totalCollateral: Satoshis,
      fundingInputs: Vector[OutputReference],
      changeAddress: Bech32Address,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    override def toJson: Value = {
      val contractInfosJson =
        contractInfo
          .map(
            info =>
              mutable.LinkedHashMap("sha256" -> Str(info._1.hex),
                                    "sats" -> Num(info._2.toLong)))

      val fundingInputsJson =
        fundingInputs
          .map(
            input =>
              mutable.LinkedHashMap("outpoint" -> Str(input.outPoint.hex),
                                    "output" -> Str(input.output.hex)))

      val timeoutsJson =
        mutable.LinkedHashMap(
          "penalty" -> Num(timeouts.penaltyTimeout.toLong),
          "contractMaturity" -> Num(timeouts.contractMaturity.toUInt32.toLong),
          "contractTimeout" -> Num(timeouts.contractTimeout.toUInt32.toLong)
        )

      val pubKeysJson =
        mutable.LinkedHashMap(
          "fundingKey" -> Str(pubKeys.fundingKey.hex),
          "toLocalCETKey" -> Str(pubKeys.toLocalCETKey.hex),
          "toRemoteCETKey" -> Str(pubKeys.toRemoteCETKey.hex),
          "finalAddress" -> Str(pubKeys.finalAddress.value)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "contractInfo" -> contractInfosJson,
          "oracleInfo" -> Str(oracleInfo.hex),
          "pubKeys" -> pubKeysJson,
          "totalCollateral" -> Num(totalCollateral.toLong),
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "feeRate" -> Num(feeRate.toLong),
          "timeouts" -> timeoutsJson
        )
      )
    }
  }

  object DLCOffer {

    def fromJson(js: Value): DLCOffer = {
      val vec = js.obj.toVector

      val contractInfoMap =
        vec
          .find(_._1 == "contractInfo")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val sha256 =
                  getValue("sha256")
                val sats = getValue("sats")

                (Sha256DigestBE(sha256.str), Satoshis(sats.num.toLong))
              }
          }
          .get
          .toMap

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val outpoint = getValue("outpoint")
                val output = getValue("output")

                OutputReference(TransactionOutPoint(outpoint.str),
                                TransactionOutput(output.str))
              }
          }
          .get
          .toVector

      val oracleInfo =
        vec.find(_._1 == "oracleInfo").map(obj => OracleInfo(obj._2.str)).get

      val pubKeys =
        vec
          .find(_._1 == "pubKeys")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val fundingKey = getValue("fundingKey")
              val toLocalCETKey = getValue("toLocalCETKey")
              val toRemoteCETKey = getValue("toRemoteCETKey")
              val finalAddress = getValue("finalAddress")

              DLCPublicKeys(
                ECPublicKey(fundingKey.str),
                ECPublicKey(toLocalCETKey.str),
                ECPublicKey(toRemoteCETKey.str),
                BitcoinAddress(finalAddress.str).get
              )
          }
          .get

      val totalCollateral = vec
        .find(_._1 == "totalCollateral")
        .map(obj => Satoshis(obj._2.num.toLong))
        .get
      val changeAddress =
        vec
          .find(_._1 == "changeAddress")
          .map(obj => Bech32Address.fromString(obj._2.str).get)
          .get
      val feeRate =
        vec
          .find(_._1 == "feeRate")
          .map(obj => SatoshisPerVirtualByte(Satoshis(obj._2.num.toLong)))
          .get

      val timeouts =
        vec
          .find(_._1 == "timeouts")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj
              val penalty = getValue("penalty")
              val contractMaturity = getValue("contractMaturity")
              val contractTimeout = getValue("contractTimeout")

              DLCTimeouts(
                UInt32(penalty.num.toLong),
                BlockTime(UInt32(contractMaturity.num.toLong)),
                BlockTime(UInt32(contractTimeout.num.toLong))
              )
          }
          .get

      DLCOffer(ContractInfo(contractInfoMap),
               oracleInfo,
               pubKeys,
               totalCollateral,
               fundingInputs,
               changeAddress,
               feeRate,
               timeouts)

    }
  }

  case class DLCAccept(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[OutputReference],
      changeAddress: Bech32Address,
      cetSigs: CETSignatures,
      eventId: Sha256DigestBE)
      extends DLCSetupMessage {

    def toJson: Value = {
      val fundingInputsJson =
        fundingInputs.map(
          input =>
            mutable.LinkedHashMap("outpoint" -> Str(input.outPoint.hex),
                                  "output" -> Str(input.output.hex)))

      val cetSigsJson =
        mutable.LinkedHashMap("winSig" -> Str(cetSigs.winSig.hex),
                              "loseSig" -> Str(cetSigs.loseSig.hex),
                              "refundSig" -> Str(cetSigs.refundSig.hex))

      val pubKeysJson =
        mutable.LinkedHashMap(
          "fundingKey" -> Str(pubKeys.fundingKey.hex),
          "toLocalCETKey" -> Str(pubKeys.toLocalCETKey.hex),
          "toRemoteCETKey" -> Str(pubKeys.toRemoteCETKey.hex),
          "finalAddress" -> Str(pubKeys.finalAddress.value)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "totalCollateral" -> Num(totalCollateral.toLong),
          "pubKeys" -> pubKeysJson,
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "cetSigs" -> cetSigsJson,
          "eventId" -> Str(eventId.hex)
        )
      )
    }
  }

  object DLCAccept {

    def fromJson(js: Value): DLCAccept = {
      val vec = js.obj.toVector

      val totalCollateral = vec
        .find(_._1 == "totalCollateral")
        .map(obj => Satoshis(obj._2.num.toLong))
        .get
      val changeAddress =
        vec
          .find(_._1 == "changeAddress")
          .map(obj => Bech32Address.fromString(obj._2.str).get)
          .get

      val pubKeys =
        vec
          .find(_._1 == "pubKeys")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val fundingKey =
                getValue("fundingKey")
              val toLocalCETKey =
                getValue("toLocalCETKey")
              val toRemoteCETKey =
                getValue("toRemoteCETKey")
              val finalAddress =
                getValue("finalAddress")

              DLCPublicKeys(
                ECPublicKey(fundingKey.str),
                ECPublicKey(toLocalCETKey.str),
                ECPublicKey(toRemoteCETKey.str),
                BitcoinAddress(finalAddress.str).get
              )
          }
          .get

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val outpoint = getValue("outpoint")
                val output = getValue("output")

                OutputReference(TransactionOutPoint(outpoint.str),
                                TransactionOutput(output.str))
              }
          }
          .get
          .toVector

      val cetSigs =
        vec
          .find(_._1 == "cetSigs")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val winSig = getValue("winSig")
              val loseSig = getValue("loseSig")
              val refundSig = getValue("refundSig")

              CETSignatures(
                PartialSignature(winSig.str),
                PartialSignature(loseSig.str),
                PartialSignature(refundSig.str)
              )
          }
          .get

      val eventId =
        vec.find(_._1 == "eventId").map(obj => Sha256DigestBE(obj._2.str)).get

      DLCAccept(totalCollateral,
                pubKeys,
                fundingInputs,
                changeAddress,
                cetSigs,
                eventId)
    }
  }

  case class DLCSign(
      cetSigs: CETSignatures,
      fundingSigs: FundingSignatures,
      eventId: Sha256DigestBE)
      extends DLCMessage {

    def toJson: Value = {
      val fundingSigsJson = fundingSigs.sigs.map(sig => Str(sig.hex))

      val cetSigsJson =
        mutable.LinkedHashMap("winSig" -> Str(cetSigs.winSig.hex),
                              "loseSig" -> Str(cetSigs.loseSig.hex),
                              "refundSig" -> Str(cetSigs.refundSig.hex))

      Obj(
        mutable.LinkedHashMap[String, Value](
          "cetSigs" -> cetSigsJson,
          "fundingSigs" -> fundingSigsJson,
          "eventId" -> Str(eventId.hex)
        )
      )
    }
  }

  object DLCSign {

    def fromJson(js: Value): DLCSign = {
      val vec = js.obj.toVector

      val cetSigs =
        vec
          .find(_._1 == "cetSigs")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val winSig = getValue("winSig")
              val loseSig = getValue("loseSig")
              val refundSig = getValue("refundSig")

              CETSignatures(
                PartialSignature(winSig.str),
                PartialSignature(loseSig.str),
                PartialSignature(refundSig.str)
              )
          }
          .get

      val fundingSigs =
        vec
          .find(_._1 == "fundingSigs")
          .map {
            case (_, value) =>
              if (value.arr.isEmpty) {
                throw new RuntimeException(
                  s"DLC Sign cannot have empty fundingSigs, got $js")
              } else {
                value.arr.map(subVal => PartialSignature(subVal.str))
              }
          }
          .get
          .toVector

      val eventId =
        vec.find(_._1 == "eventId").map(obj => Sha256DigestBE(obj._2.str)).get

      DLCSign(cetSigs, FundingSignatures(fundingSigs), eventId)
    }
  }

  case class DLCMutualCloseSig(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      mutualSig: PartialSignature)
      extends DLCMessage {
    override def toJson: Value = {
      Obj(
        mutable.LinkedHashMap[String, Value](
          "eventId" -> Str(eventId.hex),
          "oracleSig" -> Str(oracleSig.hex),
          "mutualCloseSig" -> Str(mutualSig.hex)
        )
      )
    }
  }

  object DLCMutualCloseSig {

    def fromJson(js: Value): DLCMutualCloseSig = {
      val vec = js.obj.toVector

      val eventId =
        vec.find(_._1 == "eventId").map(obj => Sha256DigestBE(obj._2.str)).get

      val oracleSig = vec
        .find(_._1 == "oracleSig")
        .map(obj => SchnorrDigitalSignature(obj._2.str))
        .get

      val sig = vec
        .find(_._1 == "mutualCloseSig")
        .map(obj => PartialSignature(obj._2.str))
        .get

      DLCMutualCloseSig(eventId, oracleSig, sig)
    }
  }
}
