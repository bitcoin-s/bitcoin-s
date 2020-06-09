package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.MapWrapper
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import play.api.libs.json._

import scala.annotation.tailrec

sealed trait DLCMessage {
  def toJson: JsValue
  def toJsonStr: String = toJson.toString()

  def eventId: Sha256DigestBE
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
  /*
  private def getValue(key: String)(
      implicit obj: mutable.LinkedHashMap[String, Value]): Value = {
    val index = obj.keys.toList.indexOf(key)
    obj.values(index)
  }
   */
  case class OracleInfo(pubKey: SchnorrPublicKey, rValue: SchnorrNonce)
      extends NetworkElement {

    override def bytes: ByteVector = pubKey.bytes ++ rValue.bytes
  }

  object OracleInfo extends Factory[OracleInfo] {

    val dummy: OracleInfo = OracleInfo(ByteVector.fill(64)(1))

    override def fromBytes(bytes: ByteVector): OracleInfo = {
      require(bytes.size == 64, s"OracleInfo is only 64 bytes, got $bytes")

      val pubkey = SchnorrPublicKey(bytes.take(32))
      val rValue = SchnorrNonce(bytes.drop(32))

      OracleInfo(pubkey, rValue)
    }
  }

  case class ContractInfo(outcomeValueMap: Map[Sha256DigestBE, Satoshis])
      extends NetworkElement
      with MapWrapper[Sha256DigestBE, Satoshis] {
    override def wrapped: Map[Sha256DigestBE, Satoshis] = outcomeValueMap

    override def bytes: ByteVector = {
      outcomeValueMap.foldLeft(ByteVector.empty) {
        case (vec, (digest, sats)) => vec ++ digest.bytes ++ sats.bytes
      }
    }
  }

  object ContractInfo extends Factory[ContractInfo] {

    private val sizeOfMapElement: Int = 40

    val empty: ContractInfo = ContractInfo(ByteVector.low(sizeOfMapElement))

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
    def changeAddress: BitcoinAddress
    require(
      totalCollateral >= Satoshis.zero,
      s"Cannot have a negative totalCollateral, got: ${totalCollateral.toLong}")
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
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    val eventId: Sha256DigestBE =
      calcEventId(oracleInfo, contractInfo, timeouts)

    override def toJson: JsValue = {
      Json.toJson(this)
    }
  }

  object DLCOffer {

    def fromJson(js: JsValue): DLCOffer = {
      js.as[DLCOffer]
    }
  }

  case class DLCAccept(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[OutputReference],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures,
      eventId: Sha256DigestBE)
      extends DLCSetupMessage {

    override def toJson: JsValue = {
      Json.toJson(this)
    }
  }

  object DLCAccept {

    def fromJson(js: JsValue): DLCAccept = {
      js.as[DLCAccept]
    }
  }

  case class DLCSign(
      cetSigs: CETSignatures,
      fundingSigs: FundingSignatures,
      eventId: Sha256DigestBE)
      extends DLCMessage {

    override def toJson: JsValue = {
      Json.toJson(this)
    }
  }

  object DLCSign {

    def fromJson(js: JsValue): DLCSign = {
      js.as[DLCSign]
    }
  }

  case class DLCMutualCloseSig(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      mutualSig: PartialSignature)
      extends DLCMessage {
    override def toJson: JsValue = {
      Json.toJson(this)
    }
  }

  object DLCMutualCloseSig {

    def fromJson(js: JsValue): DLCMutualCloseSig = {
      js.as[DLCMutualCloseSig]
    }
  }
}
