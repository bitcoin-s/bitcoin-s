package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  WalletCreateFundedPsbtOptions
}
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  DLCTimeouts,
  FundingSignatures
}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionInput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStampWithFuture}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPublicKey,
  SchnorrDigitalSignature,
  Sha256DigestBE
}
import play.api.libs.json._

import scala.collection.mutable

// for mapWrites below
import scala.language.implicitConversions

object JsonWriters {
  implicit object HashTypeWrites extends Writes[HashType] {
    override def writes(hash: HashType): JsValue = hash match {
      case _: SIGHASH_ALL                 => JsString("ALL")
      case _: SIGHASH_NONE                => JsString("NONE")
      case _: SIGHASH_SINGLE              => JsString("SINGLE")
      case _: SIGHASH_ALL_ANYONECANPAY    => JsString("ALL|ANYONECANPAY")
      case _: SIGHASH_NONE_ANYONECANPAY   => JsString("NONE|ANYONECANPAY")
      case _: SIGHASH_SINGLE_ANYONECANPAY => JsString("SINGLE|ANYONECANPAY")
      case _: SIGHASH_ANYONECANPAY =>
        throw new IllegalArgumentException(
          "SIGHHASH_ANYONECANPAY is not supported by the bitcoind RPC interface")
    }
  }

  implicit object BitcoinsWrites extends Writes[Bitcoins] {
    override def writes(o: Bitcoins): JsValue = JsNumber(o.toBigDecimal)
  }

  implicit object BitcoinAddressWrites extends Writes[BitcoinAddress] {
    override def writes(o: BitcoinAddress): JsValue = JsString(o.value)
  }

  implicit object DoubleSha256DigestWrites extends Writes[DoubleSha256Digest] {
    override def writes(o: DoubleSha256Digest): JsValue = JsString(o.hex)
  }

  implicit object DoubleSha256DigestBEWrites
      extends Writes[DoubleSha256DigestBE] {
    override def writes(o: DoubleSha256DigestBE): JsValue = JsString(o.hex)
  }

  implicit object Sha256DigestBEWrites extends Writes[Sha256DigestBE] {
    override def writes(o: Sha256DigestBE): JsValue = JsString(o.hex)
  }

  implicit object ECPublicKeyWrites extends Writes[ECPublicKey] {
    override def writes(o: ECPublicKey): JsValue = JsString(o.hex)
  }

  implicit object ScriptPubKeyWrites extends Writes[ScriptPubKey] {
    override def writes(o: ScriptPubKey): JsValue =
      JsString(BytesUtil.encodeHex(o.asmBytes))
  }

  implicit object TransactionInputWrites extends Writes[TransactionInput] {
    override def writes(o: TransactionInput): JsValue =
      JsObject(
        Seq(("txid", JsString(o.previousOutput.txIdBE.hex)),
            ("vout", JsNumber(o.previousOutput.vout.toLong)),
            ("sequence", JsNumber(o.sequence.toLong))))
  }

  implicit object OutputReferenceWrites extends Writes[OutputReference] {
    override def writes(o: OutputReference): JsValue = JsString(o.hex)
  }

  implicit object UInt32Writes extends Writes[UInt32] {
    override def writes(o: UInt32): JsValue = JsNumber(o.toLong)
  }

  implicit object BlockStampWithFutureWrites
      extends Writes[BlockStampWithFuture] {
    override def writes(o: BlockStampWithFuture): JsValue =
      UInt32Writes.writes(o.toUInt32)
  }

  implicit object SatoshisPerVirtualByteWrites
      extends Writes[SatoshisPerVirtualByte] {
    override def writes(o: SatoshisPerVirtualByte): JsValue =
      SatoshisWrites.writes(o.currencyUnit.satoshis)
  }

  implicit object TransactionWrites extends Writes[Transaction] {
    override def writes(o: Transaction): JsValue = JsString(o.hex)
  }

  implicit def mapWrites[K, V](keyString: K => String)(
      implicit
      vWrites: Writes[V]): Writes[Map[K, V]] =
    new Writes[Map[K, V]] {
      override def writes(o: Map[K, V]): JsValue =
        Json.toJson(o.map { case (k, v) => (keyString(k), v) })
    }

  implicit object MilliSatoshisWrites extends Writes[MilliSatoshis] {
    override def writes(o: MilliSatoshis): JsValue = JsNumber(o.toBigDecimal)
  }

  implicit object SatoshisWrites extends Writes[Satoshis] {
    override def writes(o: Satoshis): JsValue = JsNumber(o.toLong)
  }

  implicit object AddressTypeWrites extends Writes[AddressType] {
    override def writes(addr: AddressType): JsValue = JsString(addr.toString)
  }

  implicit object WalletCreateFundedPsbtOptionsWrites
      extends Writes[WalletCreateFundedPsbtOptions] {
    override def writes(opts: WalletCreateFundedPsbtOptions): JsValue = {
      val jsOpts: mutable.Map[String, JsValue] = mutable.Map(
        "includeWatching" -> JsBoolean(opts.includeWatching),
        "lockUnspents" -> JsBoolean(opts.lockUnspents),
        "replaceable" -> JsBoolean(opts.replaceable),
        "estimate_mode" -> JsString(opts.estimateMode.toString)
      )

      def addToMapIfDefined[T](key: String, opt: Option[T])(
          implicit writes: Writes[T]): Unit =
        opt.foreach(o => jsOpts += (key -> Json.toJson(o)))

      addToMapIfDefined("changeAddress", opts.changeAddress)
      addToMapIfDefined("changePosition", opts.changePosition)
      addToMapIfDefined("change_type", opts.changeType)
      addToMapIfDefined("feeRate", opts.feeRate)
      addToMapIfDefined("subtractFeeFromOutputs", opts.subtractFeeFromOutputs)
      addToMapIfDefined("conf_target", opts.confTarget)

      JsObject(jsOpts)
    }
  }

  implicit object SchnorrDigitalSignatureWrites
      extends Writes[SchnorrDigitalSignature] {
    override def writes(o: SchnorrDigitalSignature): JsValue = JsString(o.hex)
  }

  implicit object PartialSignatureWrites extends Writes[PartialSignature] {
    override def writes(o: PartialSignature): JsValue = JsString(o.hex)
  }

  implicit object OracleInfoWrites extends Writes[OracleInfo] {
    override def writes(info: OracleInfo): JsValue = JsString(info.hex)
  }

  implicit object ContractInfoWrites extends Writes[ContractInfo] {
    override def writes(info: ContractInfo): JsValue =
      Json.toJson(info.outcomeValueMap)(mapWrites(_.hex))
  }

  implicit val dlcTimeoutsWrites: Writes[DLCTimeouts] = Json.writes[DLCTimeouts]

  implicit val dlcPublicKeysWrites: Writes[DLCPublicKeys] =
    Json.writes[DLCPublicKeys]

  implicit val cetSignaturesWrites: Writes[CETSignatures] =
    Json.writes[CETSignatures]

  implicit val fundingSignaturesWrites: Writes[FundingSignatures] =
    Writes[FundingSignatures] { sigs =>
      val stringMap = sigs.sigs.map {
        case (outPoint, partialSigs) => outPoint.hex -> partialSigs
      }
      Json.toJson(stringMap)
    }

  implicit val dlcOfferWrites: Writes[DLCOffer] = Json.writes[DLCOffer]

  implicit val dlcAcceptWrites: Writes[DLCAccept] = Json.writes[DLCAccept]

  implicit val dlcSignWrites: Writes[DLCSign] = Json.writes[DLCSign]

  implicit val dlcMutualCloseSigWrites: Writes[DLCMutualCloseSig] =
    Json.writes[DLCMutualCloseSig]
}
