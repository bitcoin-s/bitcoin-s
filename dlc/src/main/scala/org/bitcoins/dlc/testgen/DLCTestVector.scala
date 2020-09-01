package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCOffer,
  OracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{DLCPublicKeys, DLCTimeouts}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.{DLCAcceptTLV, DLCOfferTLV, DLCSignTLV}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.{SatoshisPerKW, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{P2WPKHV0InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto.{ECPrivateKey, NetworkElement}
import play.api.libs.json._

import scala.annotation.nowarn

sealed trait DLCTestVector

case class FundingInputTx(tx: Transaction, idx: Int, inputKey: ECPrivateKey) {

  val outputRef: OutputReference =
    OutputReference(TransactionOutPoint(tx.txId, UInt32(idx)), tx.outputs(idx))
}

// Currently only supports P2WPKH inputs
case class DLCPartyParams(
    collateral: CurrencyUnit,
    fundingInputTxs: Vector[FundingInputTx],
    changeAddress: BitcoinAddress,
    fundingPrivKey: ECPrivateKey,
    payoutAddress: BitcoinAddress) {

  lazy val fundingInputs: Vector[OutputReference] =
    fundingInputTxs.map(_.outputRef)

  lazy val fundingScriptSigParams: Vector[
    ScriptSignatureParams[P2WPKHV0InputInfo]] = {
    fundingInputTxs
      .map { fundingInputTx =>
        val OutputReference(outPoint, output) = fundingInputTx.outputRef
        ScriptSignatureParams(
          P2WPKHV0InputInfo(outPoint,
                            output.value,
                            fundingInputTx.inputKey.publicKey),
          fundingInputTx.tx,
          fundingInputTx.inputKey,
          HashType.sigHashAll
        )
      }
  }

  def toOffer(params: DLCParams): DLCOffer = {
    DLCOffer(
      params.contractInfo,
      params.oracleInfo,
      DLCPublicKeys(fundingPrivKey.publicKey, payoutAddress),
      collateral.satoshis,
      fundingInputs,
      changeAddress,
      SatoshisPerVirtualByte(Satoshis(params.feeRate.toLong / 250)),
      DLCTimeouts(params.contractMaturityBound, params.contractTimeout)
    )
  }
}

case class DLCParams(
    oracleInfo: OracleInfo,
    contractInfo: ContractInfo,
    contractMaturityBound: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    feeRate: SatoshisPerKW)

case class ValidTestInputs(
    params: DLCParams,
    offerParams: DLCPartyParams,
    acceptParams: DLCPartyParams) {
  lazy val calcOffer: DLCOffer = offerParams.toOffer(params)
}

case class SuccessTestVector(
    testInputs: ValidTestInputs,
    offer: DLCOfferTLV,
    accept: DLCAcceptTLV,
    sign: DLCSignTLV,
    fundingTx: Transaction,
    cets: Vector[Transaction],
    refundTx: Transaction)
    extends DLCTestVector {

  @nowarn
  def toJson: JsValue = {
    def hexWrites[T <: NetworkElement]: Writes[T] =
      Writes[T](element => JsString(element.hex))
    implicit val oracleInfoWrites: Writes[OracleInfo] = hexWrites[OracleInfo]
    implicit val contractInfoWrites: Writes[ContractInfo] =
      hexWrites[ContractInfo]
    implicit val blockTimeStampWrites: Writes[BlockTimeStamp] =
      Writes[BlockTimeStamp] { stamp => JsNumber(stamp.toUInt32.toLong) }
    implicit val satsPerKWWrites: Writes[SatoshisPerKW] =
      Writes[SatoshisPerKW] { satsPerKW => JsNumber(satsPerKW.toLong) }
    implicit val currencyUnitWrites: Writes[CurrencyUnit] =
      Writes[CurrencyUnit] { currency => JsNumber(currency.satoshis.toLong) }
    implicit val transactionWrites: Writes[Transaction] = hexWrites[Transaction]
    implicit val ecPrivKeyWrites: Writes[ECPrivateKey] = hexWrites[ECPrivateKey]
    implicit val fundingInputTxWrites: Writes[FundingInputTx] =
      Json.writes[FundingInputTx]
    implicit val addressWrites: Writes[BitcoinAddress] =
      Writes[BitcoinAddress] { address => JsString(address.toString) }
    implicit val dlcParamWrites: Writes[DLCParams] = Json.writes[DLCParams]
    implicit val dlcPartyParamWrites: Writes[DLCPartyParams] =
      Json.writes[DLCPartyParams]

    implicit val offerTLVWrites: Writes[DLCOfferTLV] = hexWrites[DLCOfferTLV]
    implicit val acceptTLVWrites: Writes[DLCAcceptTLV] = hexWrites[DLCAcceptTLV]
    implicit val signTLVWrites: Writes[DLCSignTLV] = hexWrites[DLCSignTLV]
    implicit val validTestInputsWrites: Writes[ValidTestInputs] =
      Json.writes[ValidTestInputs]
    implicit val successTestVectorWrites: Writes[SuccessTestVector] =
      Json.writes[SuccessTestVector]

    Json.toJson(this)
  }

  def toJsonStr: String = {
    Json.prettyPrint(toJson)
  }
}
