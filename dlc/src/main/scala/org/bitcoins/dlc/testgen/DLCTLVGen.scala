package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  DLCTimeouts,
  FundingSignatures
}
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{DLCAcceptTLV, DLCOfferTLV, DLCSignTLV}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import scodec.bits.ByteVector

object DLCTLVGen {

  val defaultAmt: Satoshis = CurrencyUnits.oneBTC.satoshis

  def hash(bytes: ByteVector = NumberUtil.randomBytes(32)): Sha256Digest = {
    CryptoUtil.sha256(bytes)
  }

  def genContractInfo(
      outcomes: Vector[Sha256Digest] = DLCTestUtil.genOutcomes(3),
      totalInput: CurrencyUnit = defaultAmt * 2): ContractInfo = {
    DLCTestUtil.genContractInfos(outcomes, totalInput)._1
  }

  def genOracleInfo(
      oraclePubKey: SchnorrPublicKey =
        ECPublicKey.freshPublicKey.schnorrPublicKey,
      oracleRValue: SchnorrNonce =
        ECPublicKey.freshPublicKey.schnorrNonce): OracleInfo = {
    OracleInfo(oraclePubKey, oracleRValue)
  }

  def p2wpkh(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey): P2WPKHWitnessSPKV0 = {
    P2WPKHWitnessSPKV0(pubKey)
  }

  def address(
      spk: ScriptPubKey = p2wpkh(),
      network: NetworkParameters = RegTest): BitcoinAddress = {
    spk match {
      case wspk: WitnessScriptPubKey => Bech32Address(wspk, network)
      case p2sh: P2SHScriptPubKey    => P2SHAddress(p2sh, network)
      case p2pkh: P2PKHScriptPubKey  => P2PKHAddress(p2pkh, network)
      case _: RawScriptPubKey =>
        throw new IllegalArgumentException(s"$spk is not valid for an address")
    }
  }

  def inputTransaction(
      input: CurrencyUnit = defaultAmt,
      spk: ScriptPubKey = p2wpkh()): Transaction = {
    BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector.empty,
      Vector(TransactionOutput(input * 2, spk)),
      UInt32.zero
    )
  }

  def outputReference(
      input: CurrencyUnit = defaultAmt,
      spk: ScriptPubKey =
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)): OutputReference = {
    val tx = inputTransaction(input, spk)
    OutputReference(TransactionOutPoint(tx.txIdBE, UInt32.zero),
                    tx.outputs.head)
  }

  def adaptorSig: ECAdaptorSignature = {
    ECAdaptorSignature(
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement
    )
  }

  def ecdsaSig(sigHashByte: Boolean = true): ECDigitalSignature = {
    val sigWithoutSigHash = ECDigitalSignature.fromRS(
      ECPrivateKey.freshPrivateKey.fieldElement.toBigInteger,
      ECPrivateKey.freshPrivateKey.fieldElement.toBigInteger)

    if (sigHashByte) {
      ECDigitalSignature(sigWithoutSigHash.bytes :+ 0x01)
    } else {
      sigWithoutSigHash
    }
  }

  def partialSig(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      sigHashByte: Boolean = true): PartialSignature = {
    PartialSignature(pubKey, ecdsaSig(sigHashByte))
  }

  def cetSigs(
      outcomes: Vector[Sha256Digest] = DLCTestUtil.genOutcomes(3),
      fundingPubKey: ECPublicKey =
        ECPublicKey.freshPublicKey): CETSignatures = {
    CETSignatures(outcomes.map(outcome => outcome -> adaptorSig).toMap,
                  partialSig(fundingPubKey, sigHashByte = false))
  }

  def fundingSigs(
      outPoints: Vector[TransactionOutPoint] = Vector(
        outputReference().outPoint)): FundingSignatures = {
    FundingSignatures(
      outPoints.map(outpoint => outpoint -> Vector(partialSig())).toMap)
  }

  def dlcOffer(
      contractInfo: ContractInfo = genContractInfo(),
      oracleInfo: OracleInfo = genOracleInfo(),
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      totalCollateral: Satoshis = defaultAmt,
      fundingInputs: Vector[OutputReference] = Vector(outputReference()),
      changeAddress: BitcoinAddress = address(),
      feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte.one,
      contractMaturityBound: BlockTimeStamp = BlockTimeStamp(100),
      contractTimeout: BlockTimeStamp = BlockTimeStamp(200)): DLCOffer = {
    DLCOffer(
      contractInfo,
      oracleInfo,
      DLCPublicKeys(fundingPubKey, payoutAddress),
      totalCollateral,
      fundingInputs,
      changeAddress,
      feeRate,
      DLCTimeouts(contractMaturityBound, contractTimeout)
    )
  }

  def dlcOfferTLV(
      contractInfo: ContractInfo = genContractInfo(),
      oracleInfo: OracleInfo = genOracleInfo(),
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      totalCollateral: Satoshis = defaultAmt,
      fundingInputs: Vector[OutputReference] = Vector(outputReference()),
      changeAddress: BitcoinAddress = address(),
      feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte.one,
      contractMaturityBound: BlockTimeStamp = BlockTimeStamp(100),
      contractTimeout: BlockTimeStamp = BlockTimeStamp(200)): DLCOfferTLV = {
    dlcOffer(contractInfo,
             oracleInfo,
             fundingPubKey,
             payoutAddress,
             totalCollateral,
             fundingInputs,
             changeAddress,
             feeRate,
             contractMaturityBound,
             contractTimeout).toTLV
  }

  def dlcAccept(
      totalCollateral: Satoshis = defaultAmt,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      fundingInputs: Vector[OutputReference] = Vector(outputReference()),
      changeAddress: BitcoinAddress = address(),
      cetSignatures: CETSignatures = cetSigs(),
      tempContractId: Sha256Digest = hash()): DLCAccept = {
    DLCAccept(totalCollateral,
              DLCPublicKeys(fundingPubKey, payoutAddress),
              fundingInputs,
              changeAddress,
              cetSignatures,
              tempContractId)
  }

  def dlcAcceptTLV(
      totalCollateral: Satoshis = defaultAmt,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      fundingInputs: Vector[OutputReference] = Vector(outputReference()),
      changeAddress: BitcoinAddress = address(),
      cetSignatures: CETSignatures = cetSigs(),
      tempContractId: Sha256Digest = hash()): DLCAcceptTLV = {
    dlcAccept(totalCollateral,
              fundingPubKey,
              payoutAddress,
              fundingInputs,
              changeAddress,
              cetSignatures,
              tempContractId).toTLV
  }

  def dlcAcceptFromOffer(
      offer: DLCOffer,
      overCollateral: Satoshis = Satoshis.zero,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      fundingInputs: Vector[OutputReference] = Vector(outputReference()),
      changeAddress: BitcoinAddress = address()): DLCAccept = {
    val totalCollateral =
      offer.contractInfo.values.max - offer.totalCollateral + overCollateral

    val cetSignatures =
      cetSigs(offer.contractInfo.outcomeValueMap.keys.toVector, fundingPubKey)

    val tempContractId = offer.tempContractId

    DLCAccept(totalCollateral.satoshis,
              DLCPublicKeys(fundingPubKey, payoutAddress),
              fundingInputs,
              changeAddress,
              cetSignatures,
              tempContractId)
  }

  def dlcAcceptTLVFromOffer(
      offer: DLCOffer,
      overCollateral: Satoshis = Satoshis.zero,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      fundingInputs: Vector[OutputReference] = Vector(outputReference()),
      changeAddress: BitcoinAddress = address()): DLCAcceptTLV = {
    dlcAcceptFromOffer(offer,
                       overCollateral,
                       fundingPubKey,
                       payoutAddress,
                       fundingInputs,
                       changeAddress).toTLV
  }

  def dlcSign(
      cetSignatures: CETSignatures = cetSigs(),
      fundingSignatures: FundingSignatures = fundingSigs(),
      contractId: ByteVector = hash().bytes): DLCSign = {
    DLCSign(cetSignatures, fundingSignatures, contractId)
  }

  def dlcSignTLV(
      cetSignatures: CETSignatures = cetSigs(),
      fundingSignatures: FundingSignatures = fundingSigs(),
      contractId: ByteVector = hash().bytes): DLCSignTLV = {
    dlcSign(cetSignatures, fundingSignatures, contractId).toTLV
  }

  def dlcSignFromOffer(
      offer: DLCOffer,
      contractId: ByteVector = hash().bytes): DLCSign = {
    val cetSignatures = cetSigs(
      offer.contractInfo.outcomeValueMap.keys.toVector,
      offer.pubKeys.fundingKey)
    val fundingSignatures = fundingSigs(offer.fundingInputs.map(_.outPoint))
    DLCSign(cetSignatures, fundingSignatures, contractId)
  }

  def dlcSignTLVFromOffer(
      offer: DLCOffer,
      contractId: ByteVector = hash().bytes): DLCSignTLV = {
    dlcSignFromOffer(offer, contractId).toTLV
  }

  def dlcSignFromOfferAndAccept(offer: DLCOffer, accept: DLCAccept): DLCSign = {
    import scala.concurrent.{Await, ExecutionContext}
    import scala.concurrent.duration.DurationInt

    val builder =
      DLCTxBuilder(offer, accept.withoutSigs)(ExecutionContext.global)
    val fundingTx = Await.result(builder.buildFundingTx, 5.seconds)
    val contractId = fundingTx.txIdBE.bytes.xor(accept.tempContractId.bytes)

    dlcSignFromOffer(offer, contractId)
  }

  def dlcSignTLVFromOfferAndAccept(
      offer: DLCOffer,
      accept: DLCAccept): DLCSignTLV = {
    dlcSignFromOfferAndAccept(offer, accept).toTLV
  }
}
