package org.bitcoins.dlc.testgen

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.wallet.builder.{
  DualFundingInput,
  DualFundingTxFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.ECPublicKey
import play.api.libs.json._
import scodec.bits.ByteVector

case class DLCFeeTestVector(
    inputs: DLCFeeTestVectorInput,
    offerFundingFee: Satoshis,
    offerClosingFee: Satoshis,
    acceptFundingFee: Satoshis,
    acceptClosingFee: Satoshis)
    extends TestVector {

  override def toJson: JsValue = {
    Json.toJson(this)(DLCFeeTestVector.dlcFeeTestVectorFormat)
  }
}

case class FundingFeeInfo(redeemScriptLen: Int, maxWitnessLen: Int) {

  lazy val mockDualFundingInput: DualFundingInput = {
    val scriptSig = if (redeemScriptLen == 0) {
      EmptyScriptSignature
    } else {
      val wspk = if (redeemScriptLen == 22) {
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
      } else {
        P2WSHWitnessSPKV0(EmptyScriptPubKey)
      }
      P2SHScriptSignature(wspk)
    }

    DualFundingInput(scriptSig, maxWitnessLen)
  }
}

case class DLCFeeTestVectorInput(
    offerInputs: Vector[FundingFeeInfo],
    offerPayoutSPKLen: Int,
    offerChangeSPKLen: Int,
    acceptInputs: Vector[FundingFeeInfo],
    acceptPayoutSPKLen: Int,
    acceptChangeSPKLen: Int,
    feeRate: SatoshisPerVirtualByte) {

  lazy val mockDualFundingTxFinalizer: DualFundingTxFinalizer = {
    def mockSPK(len: Int): ScriptPubKey = {
      ScriptPubKey.fromAsmBytes(ByteVector.fill(len)(0x00.toByte))
    }

    DualFundingTxFinalizer(
      offerInputs.map(_.mockDualFundingInput),
      mockSPK(offerPayoutSPKLen),
      mockSPK(offerChangeSPKLen),
      acceptInputs.map(_.mockDualFundingInput),
      mockSPK(acceptPayoutSPKLen),
      mockSPK(acceptChangeSPKLen),
      feeRate,
      EmptyScriptPubKey
    )
  }

  lazy val offerFundingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.offerFundingFee

  lazy val offerClosingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.offerFutureFee

  lazy val acceptFundingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.acceptFundingFee

  lazy val acceptClosingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.acceptFutureFee
}

object DLCFeeTestVectorInput {

  def fromJson(json: JsValue): JsResult[DLCFeeTestVectorInput] = {
    json.validate(DLCFeeTestVector.dlcFeeTestVectorInputFormat)
  }
}

object DLCFeeTestVector extends TestVectorParser[DLCFeeTestVector] {

  def apply(inputs: DLCFeeTestVectorInput): DLCFeeTestVector = {
    DLCFeeTestVector.apply(
      inputs,
      inputs.offerFundingFee.satoshis,
      inputs.offerClosingFee.satoshis,
      inputs.acceptFundingFee.satoshis,
      inputs.acceptClosingFee.satoshis
    )
  }

  def apply(
      offerInputs: Vector[FundingFeeInfo],
      offerPayoutSPKLen: Int,
      offerChangeSPKLen: Int,
      acceptInputs: Vector[FundingFeeInfo],
      acceptPayoutSPKLen: Int,
      acceptChangeSPKLen: Int,
      feeRate: SatoshisPerVirtualByte): DLCFeeTestVector = {
    DLCFeeTestVector(
      DLCFeeTestVectorInput(
        offerInputs,
        offerPayoutSPKLen,
        offerChangeSPKLen,
        acceptInputs,
        acceptPayoutSPKLen,
        acceptChangeSPKLen,
        feeRate
      )
    )
  }

  implicit val fundingFeeInfoFormat: Format[FundingFeeInfo] =
    Json.format[FundingFeeInfo]

  implicit val satsPerVBFormat: Format[SatoshisPerVirtualByte] =
    Format[SatoshisPerVirtualByte](
      {
        _.validate[Long].map(Satoshis.apply).map(SatoshisPerVirtualByte.apply)
      },
      { satsPerVB => JsNumber(satsPerVB.toLong) }
    )

  implicit val dlcFeeTestVectorInputFormat: Format[DLCFeeTestVectorInput] =
    Json.format[DLCFeeTestVectorInput]

  implicit val satoshisFormat: Format[Satoshis] =
    Format[Satoshis](
      { _.validate[Long].map(Satoshis.apply) },
      { sats => JsNumber(sats.toLong) }
    )

  implicit val dlcFeeTestVectorFormat: Format[DLCFeeTestVector] =
    Json.format[DLCFeeTestVector]

  override def fromJson(json: JsValue): JsResult[DLCFeeTestVector] = {
    json.validate[DLCFeeTestVector]
  }
}
