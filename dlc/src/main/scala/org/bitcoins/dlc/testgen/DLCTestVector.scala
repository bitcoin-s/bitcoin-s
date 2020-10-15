package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCAcceptWithoutSigs,
  DLCOffer,
  OracleAndContractInfo,
  OracleInfo,
  SingleNonceOracleAndContractInfo,
  SingleNonceOracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{
  DLCFundingInput,
  DLCMessage,
  DLCPublicKeys,
  DLCTimeouts
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  ScriptWitness,
  ScriptWitnessV0,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import play.api.libs.json._
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

sealed trait DLCTestVector[Outcome <: DLCOutcomeType] extends TestVector

case class DLCTestVectorHelper[Outcome <: DLCOutcomeType]()
    extends TestVectorParser[DLCTestVector[Outcome]] {

  def fromJson(json: JsValue): JsResult[DLCTestVector[Outcome]] = {
    TestVectorHelper[Outcome]().fromJson(json)
  }
}

case class FundingInputTx(
    tx: Transaction,
    idx: Int,
    inputKeys: Vector[ECPrivateKey],
    redeemScript: Option[WitnessScriptPubKey],
    scriptWitness: ScriptWitnessV0) {

  val outputRef: OutputReference =
    OutputReference(TransactionOutPoint(tx.txId, UInt32(idx)), tx.outputs(idx))

  lazy val scriptSignatureParams: ScriptSignatureParams[InputInfo] = {
    ScriptSignatureParams(
      InputInfo(TransactionOutPoint(tx.txId, UInt32(idx)),
                tx.outputs(idx),
                redeemScript,
                Some(scriptWitness),
                ConditionalPath.NoCondition),
      tx,
      inputKeys,
      HashType.sigHashAll
    )
  }

  def toFundingInput(implicit ec: ExecutionContext): DLCFundingInput = {
    DLCFundingInput.fromInputSigningInfo(scriptSignatureParams)
  }

  def toSerializedFundingInputTx(implicit
      ec: ExecutionContext): SerializedFundingInputTx = {
    SerializedFundingInputTx(tx,
                             idx,
                             inputKeys,
                             redeemScript,
                             scriptWitness,
                             scriptSignatureParams.maxWitnessLen)
  }
}

case class SerializedFundingInputTx(
    tx: Transaction,
    idx: Int,
    inputKeys: Vector[ECPrivateKey],
    redeemScript: Option[WitnessScriptPubKey],
    scriptWitness: ScriptWitnessV0,
    maxWitnessLen: Int) {

  def toFundingInputTx: FundingInputTx = {
    FundingInputTx(tx, idx, inputKeys, redeemScript, scriptWitness)
  }
}

case class DLCPartyParams(
    collateral: CurrencyUnit,
    fundingInputTxs: Vector[FundingInputTx],
    changeAddress: BitcoinAddress,
    fundingPrivKey: ECPrivateKey,
    payoutAddress: BitcoinAddress) {

  def fundingInputs(implicit ec: ExecutionContext): Vector[DLCFundingInput] =
    fundingInputTxs.map(_.toFundingInput)

  lazy val fundingScriptSigParams: Vector[ScriptSignatureParams[InputInfo]] = {
    fundingInputTxs.map(_.scriptSignatureParams)
  }

  def toOffer[Outcome <: DLCOutcomeType](params: DLCParams[Outcome])(implicit
      ec: ExecutionContext): DLCOffer[Outcome] = {
    val oracleAndContractInfo = params.oracleInfo match {
      case info: DLCMessage.SingleNonceOracleInfo =>
        SingleNonceOracleAndContractInfo(
          info,
          ContractInfo(params.contractInfo.map(_.toMapEntry).toMap))
    }

    DLCOffer(
      oracleAndContractInfo.asInstanceOf[OracleAndContractInfo[Outcome]],
      DLCPublicKeys(fundingPrivKey.publicKey, payoutAddress),
      collateral.satoshis,
      fundingInputs,
      changeAddress,
      params.feeRate,
      DLCTimeouts(params.contractMaturityBound, params.contractTimeout)
    )
  }
}

case class SerializedContractInfoEntry(
    preImage: String,
    outcome: Sha256Digest,
    localPayout: CurrencyUnit) {

  def toMapEntry: (EnumOutcome, Satoshis) = {
    EnumOutcome(preImage) -> localPayout.satoshis
  }
}

object SerializedContractInfoEntry {

  def fromContractInfo(
      contractInfo: ContractInfo): Vector[SerializedContractInfoEntry] = {
    contractInfo.map {
      case (str, amt) =>
        SerializedContractInfoEntry(str.outcome,
                                    CryptoUtil.sha256(str.outcome),
                                    amt)
    }.toVector
  }
}

case class DLCParams[Outcome <: DLCOutcomeType](
    oracleInfo: OracleInfo[Outcome],
    contractInfo: Vector[SerializedContractInfoEntry],
    contractMaturityBound: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    feeRate: SatoshisPerVirtualByte,
    realOutcome: Sha256Digest,
    oracleSignature: SchnorrDigitalSignature)

case class ValidTestInputs[Outcome <: DLCOutcomeType](
    params: DLCParams[Outcome],
    offerParams: DLCPartyParams,
    acceptParams: DLCPartyParams) {

  def offer(implicit ec: ExecutionContext): DLCOffer[Outcome] =
    offerParams.toOffer(params)

  def accept(implicit ec: ExecutionContext): DLCAcceptWithoutSigs =
    DLCAcceptWithoutSigs(
      acceptParams.collateral.satoshis,
      DLCPublicKeys(acceptParams.fundingPrivKey.publicKey,
                    acceptParams.payoutAddress),
      acceptParams.fundingInputs,
      acceptParams.changeAddress,
      offer.tempContractId
    )

  def builder(implicit ec: ExecutionContext): DLCTxBuilder[Outcome] =
    DLCTxBuilder(offer, accept)

  def buildTransactions(implicit
      ec: ExecutionContext): Future[DLCTransactions] = {
    val builder = this.builder
    for {
      fundingTx <- builder.buildFundingTx
      cetFs =
        params.contractInfo
          .map(entry => EnumOutcome(entry.preImage).asInstanceOf[Outcome])
          .map(builder.buildCET)
      cets <- Future.sequence(cetFs)
      refundTx <- builder.buildRefundTx
    } yield DLCTransactions(fundingTx, cets, refundTx)
  }
}

case class ValidTestInputsHelper[Outcome <: DLCOutcomeType]() {

  def fromJson(json: JsValue): JsResult[ValidTestInputs[Outcome]] = {
    Json.fromJson(json)(TestVectorHelper[Outcome]().validTestInputsFormat)
  }
}

case class DLCTransactions(
    fundingTx: Transaction,
    cets: Vector[Transaction],
    refundTx: Transaction)

case class SuccessTestVector[Outcome <: DLCOutcomeType](
    testInputs: ValidTestInputs[Outcome],
    offer: LnMessage[DLCOfferTLV[Outcome]],
    accept: LnMessage[DLCAcceptTLV],
    sign: LnMessage[DLCSignTLV],
    unsignedTxs: DLCTransactions,
    signedTxs: DLCTransactions)
    extends DLCTestVector[Outcome] {

  implicit val outcomeFmt: Format[Outcome] =
    testInputs.params.oracleInfo match {
      case _: SingleNonceOracleInfo =>
        Format[EnumOutcome](
          { _.validate[String].map(EnumOutcome.apply) },
          { enumOutcome => JsString(enumOutcome.outcome) }
        ).asInstanceOf[Format[Outcome]]
    }

  override def toJson: JsValue = {
    Json.toJson(this)(TestVectorHelper[Outcome]().successTestVectorFormat)
  }
}

case class TestVectorHelper[Outcome <: DLCOutcomeType]()
    extends TestVectorParser[SuccessTestVector[Outcome]] {

  def hexFormat[T <: NetworkElement](factory: Factory[T]): Format[T] =
    Format[T](
      { hex => hex.validate[String].map(factory.fromHex) },
      { element => JsString(element.hex) }
    )

  implicit val singleNonceOracleInfoFormat: Format[SingleNonceOracleInfo] =
    Format[SingleNonceOracleInfo](
      {
        _.validate[Map[String, String]]
          .map(map =>
            SingleNonceOracleInfo(SchnorrPublicKey(map("publicKey")),
                                  SchnorrNonce(map("nonce"))))
      },
      { info =>
        Json.toJson(
          Map("publicKey" -> info.pubKey.hex, "nonce" -> info.rValue.hex))
      }
    )

  implicit val oracleInfoFormat: Format[OracleInfo[Outcome]] =
    Format[OracleInfo[Outcome]](
      { json: JsValue => json.validate[SingleNonceOracleInfo] }
        .asInstanceOf[Reads[OracleInfo[Outcome]]],
      {
        case info: SingleNonceOracleInfo =>
          singleNonceOracleInfoFormat.writes(info)
      }
    )

  implicit val blockTimeStampFormat: Format[BlockTimeStamp] =
    Format[BlockTimeStamp](
      { _.validate[Long].map(UInt32.apply).map(BlockTimeStamp.apply) },
      { stamp => JsNumber(stamp.toUInt32.toLong) }
    )

  implicit val satsPerVBFormat: Format[SatoshisPerVirtualByte] =
    Format[SatoshisPerVirtualByte](
      {
        _.validate[Long].map(Satoshis.apply).map(SatoshisPerVirtualByte.apply)
      },
      { satsPerVB => JsNumber(satsPerVB.toLong) }
    )

  implicit val sha256DigestFormat: Format[Sha256Digest] = hexFormat(
    Sha256Digest)

  implicit val schnorrDigitalSignatureFormat: Format[SchnorrDigitalSignature] =
    hexFormat(SchnorrDigitalSignature)

  implicit val currencyUnitFormat: Format[CurrencyUnit] =
    Format[CurrencyUnit](
      { _.validate[Long].map(Satoshis.apply) },
      { currency => JsNumber(currency.satoshis.toLong) }
    )
  implicit val transactionFormat: Format[Transaction] = hexFormat(Transaction)
  implicit val ecPrivKeyFormat: Format[ECPrivateKey] = hexFormat(ECPrivateKey)

  implicit val witnessScriptPubKeyFormat: Format[WitnessScriptPubKey] =
    Format[WitnessScriptPubKey](
      { json => json.validate[String].map(WitnessScriptPubKey.fromAsmHex) },
      { wspk => JsString(wspk.asmBytes.toHex) }
    )

  implicit val scriptWitnessV0Format: Format[ScriptWitnessV0] =
    Format[ScriptWitnessV0](
      {
        _.validate[String]
          .map(ByteVector.fromValidHex(_))
          .map(ScriptWitness.fromBytes)
          .map(_.asInstanceOf[ScriptWitnessV0])
      },
      { witness => JsString(witness.hex) }
    )

  implicit val serializedFundingInputTx: Format[SerializedFundingInputTx] =
    Json.format[SerializedFundingInputTx]

  implicit val fundingInputTxFormat: Format[FundingInputTx] =
    Format[FundingInputTx](
      { _.validate[SerializedFundingInputTx].map(_.toFundingInputTx) },
      { inputTx =>
        Json.toJson(inputTx.toSerializedFundingInputTx(ExecutionContext.global))
      }
    )

  implicit val addressFormat: Format[BitcoinAddress] =
    Format[BitcoinAddress](
      { _.validate[String].map(BitcoinAddress.fromString) },
      { address => JsString(address.toString) }
    )

  implicit val contractInfoFormat: Format[SerializedContractInfoEntry] =
    Json.format[SerializedContractInfoEntry]

  implicit val dlcParamFormat: Format[DLCParams[Outcome]] =
    Json.format[DLCParams[Outcome]]

  implicit val DLCPartyParamsFormat: Format[DLCPartyParams] =
    Json.format[DLCPartyParams]

  implicit val offerMsgEnumFormat: Format[LnMessage[DLCOfferTLV[EnumOutcome]]] =
    hexFormat(LnMessageFactory(DLCOfferTLV))

  implicit val offerMsgFormat: Format[LnMessage[DLCOfferTLV[Outcome]]] =
    Format[LnMessage[DLCOfferTLV[Outcome]]](
      { json: JsValue => json.validate[LnMessage[DLCOfferTLV[EnumOutcome]]] }
        .asInstanceOf[Reads[LnMessage[DLCOfferTLV[Outcome]]]],
      { offer =>
        offer.tlv.oracleInfo match {
          case _: OracleInfoV0TLV =>
            offerMsgEnumFormat.writes(
              offer.asInstanceOf[LnMessage[DLCOfferTLV[EnumOutcome]]])
        }
      }
    )

  implicit val acceptMsgFormat: Format[LnMessage[DLCAcceptTLV]] = hexFormat(
    LnMessageFactory(DLCAcceptTLV))

  implicit val signMsgFormat: Format[LnMessage[DLCSignTLV]] = hexFormat(
    LnMessageFactory(DLCSignTLV))

  implicit val validTestInputsFormat: Format[ValidTestInputs[Outcome]] =
    Json.format[ValidTestInputs[Outcome]]

  implicit val dlcTransactionsFormat: Format[DLCTransactions] =
    Json.format[DLCTransactions]

  implicit val successTestVectorFormat: Format[SuccessTestVector[Outcome]] =
    Json.format[SuccessTestVector[Outcome]]

  override def fromJson(json: JsValue): JsResult[SuccessTestVector[Outcome]] = {
    json.validate[SuccessTestVector[Outcome]]
  }
}
