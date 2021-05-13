package org.bitcoins.dlc.testgen

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
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
import org.bitcoins.core.util.Indexed
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

sealed trait DLCTestVector extends TestVector

object DLCTestVector extends TestVectorParser[DLCTestVector] {

  def fromJson(json: JsValue): JsResult[DLCTestVector] = {
    SuccessTestVector.fromJson(json)
  }
}

case class FundingInputTx(
    serialId: UInt64,
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

  def toFundingInput: DLCFundingInput = {
    DLCFundingInput.fromInputSigningInfo(scriptSignatureParams, serialId)
  }

  def toSerializedFundingInputTx: SerializedFundingInputTx = {
    SerializedFundingInputTx(serialId,
                             tx,
                             idx,
                             inputKeys,
                             redeemScript,
                             scriptWitness,
                             scriptSignatureParams.maxWitnessLen)
  }
}

case class SerializedFundingInputTx(
    serialId: UInt64,
    tx: Transaction,
    idx: Int,
    inputKeys: Vector[ECPrivateKey],
    redeemScript: Option[WitnessScriptPubKey],
    scriptWitness: ScriptWitnessV0,
    maxWitnessLen: Int) {

  def toFundingInputTx: FundingInputTx = {
    FundingInputTx(serialId, tx, idx, inputKeys, redeemScript, scriptWitness)
  }
}

// Currently only supports P2WPKH inputs
case class DLCPartyParams(
    collateral: CurrencyUnit,
    fundingInputTxs: Vector[FundingInputTx],
    changeAddress: BitcoinAddress,
    changeSerialId: UInt64,
    fundingPrivKey: ECPrivateKey,
    payoutAddress: BitcoinAddress,
    payoutSerialId: UInt64,
    fundOutputSerialId: UInt64) {

  def fundingInputs: Vector[DLCFundingInput] =
    fundingInputTxs.map(_.toFundingInput)

  lazy val fundingScriptSigParams: Vector[ScriptSignatureParams[InputInfo]] = {
    fundingInputTxs.map(_.scriptSignatureParams)
  }

  def toOffer(params: DLCParams): DLCOffer = {
    DLCOffer(
      ContractInfo(
        EnumContractDescriptor(params.contractInfo.map(_.toMapEntry)),
        params.oracleInfo),
      DLCPublicKeys(fundingPrivKey.publicKey, payoutAddress),
      collateral.satoshis,
      fundingInputs,
      changeAddress,
      changeSerialId,
      payoutSerialId,
      fundOutputSerialId,
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

  def fromContractDescriptor(contractInfo: EnumContractDescriptor): Vector[
    SerializedContractInfoEntry] = {
    contractInfo.map { case (EnumOutcome(str), amt) =>
      SerializedContractInfoEntry(str,
                                  CryptoUtil.sha256DLCAttestation(str),
                                  amt)
    }.toVector
  }
}

case class DLCParams(
    oracleInfo: EnumSingleOracleInfo,
    contractInfo: Vector[SerializedContractInfoEntry],
    contractMaturityBound: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    feeRate: SatoshisPerVirtualByte,
    realOutcome: Sha256Digest,
    oracleSignature: SchnorrDigitalSignature)

case class ValidTestInputs(
    params: DLCParams,
    offerParams: DLCPartyParams,
    acceptParams: DLCPartyParams) {

  def offer: DLCOffer = offerParams.toOffer(params)

  def accept: DLCAcceptWithoutSigs =
    DLCAcceptWithoutSigs(
      acceptParams.collateral.satoshis,
      DLCPublicKeys(acceptParams.fundingPrivKey.publicKey,
                    acceptParams.payoutAddress),
      acceptParams.fundingInputs,
      acceptParams.changeAddress,
      acceptParams.payoutSerialId,
      acceptParams.changeSerialId,
      DLCAccept.NoNegotiationFields,
      offer.tempContractId
    )

  def builder: DLCTxBuilder = DLCTxBuilder(offer, accept)

  def buildTransactions: DLCTransactions = {
    val builder = this.builder
    val fundingTx = builder.buildFundingTx
    val adaptorPoints =
      params.contractInfo
        .map(_.preImage)
        .map(EnumOutcome.apply)
        .map(outcome => EnumOracleOutcome(Vector(params.oracleInfo), outcome))
        .map(_.sigPoint)
    val cets = builder.buildCETs(Indexed(adaptorPoints))
    val refundTx = builder.buildRefundTx

    DLCTransactions(fundingTx, cets, refundTx)
  }
}

object ValidTestInputs {

  def fromJson(json: JsValue): JsResult[ValidTestInputs] = {
    Json.fromJson(json)(SuccessTestVector.validTestInputsFormat)
  }
}

case class DLCTransactions(
    fundingTx: Transaction,
    cets: Vector[Transaction],
    refundTx: Transaction)

case class SuccessTestVector(
    testInputs: ValidTestInputs,
    offer: LnMessage[DLCOfferTLV],
    accept: LnMessage[DLCAcceptTLV],
    sign: LnMessage[DLCSignTLV],
    unsignedTxs: DLCTransactions,
    signedTxs: DLCTransactions)
    extends DLCTestVector {

  override def toJson: JsValue = {
    Json.toJson(this)(SuccessTestVector.successTestVectorFormat)
  }
}

object SuccessTestVector extends TestVectorParser[SuccessTestVector] {

  def hexFormat[T <: NetworkElement](factory: Factory[T]): Format[T] =
    Format[T](
      { hex => hex.validate[String].map(factory.fromHex) },
      { element => JsString(element.hex) }
    )

  implicit val u64Format: Format[UInt64] = hexFormat(UInt64)

  implicit val oracleInfoFormat: Format[EnumSingleOracleInfo] = hexFormat(
    EnumSingleOracleInfo)

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
        Json.toJson(inputTx.toSerializedFundingInputTx)
      }
    )

  implicit val addressFormat: Format[BitcoinAddress] =
    Format[BitcoinAddress](
      { _.validate[String].map(BitcoinAddress.fromString) },
      { address => JsString(address.toString) }
    )

  implicit val contractInfoFormat: Format[SerializedContractInfoEntry] =
    Json.format[SerializedContractInfoEntry]

  implicit val dlcParamFormat: Format[DLCParams] = Json.format[DLCParams]

  implicit val DLCPartyParamsFormat: Format[DLCPartyParams] =
    Json.format[DLCPartyParams]

  implicit val offerMsgFormat: Format[LnMessage[DLCOfferTLV]] = hexFormat(
    LnMessageFactory(DLCOfferTLV))

  implicit val acceptMsgFormat: Format[LnMessage[DLCAcceptTLV]] = hexFormat(
    LnMessageFactory(DLCAcceptTLV))

  implicit val signMsgFormat: Format[LnMessage[DLCSignTLV]] = hexFormat(
    LnMessageFactory(DLCSignTLV))

  implicit val validTestInputsFormat: Format[ValidTestInputs] =
    Json.format[ValidTestInputs]

  implicit val dlcTransactionsFormat: Format[DLCTransactions] =
    Json.format[DLCTransactions]

  implicit val successTestVectorFormat: Format[SuccessTestVector] =
    Json.format[SuccessTestVector]

  override def fromJson(json: JsValue): JsResult[SuccessTestVector] = {
    json.validate[SuccessTestVector]
  }
}
