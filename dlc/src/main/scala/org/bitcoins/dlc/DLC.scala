package org.bitcoins.dlc

import org.bitcoins.core.crypto.{
  TransactionSignatureChecker,
  TransactionSignatureSerializer,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.DLCMessage.{DLCAccept, DLCOffer, DLCSign}
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.{
  DualFundingTxFinalizer,
  RawTxBuilderWithFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  ECSignatureParams,
  InputInfo,
  P2WSHV0InputInfo
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCCETBuilder
import org.bitcoins.dlc.execution.{ExecutedDLCOutcome, RefundDLCOutcome}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

trait DLC {

  def buildContractDescriptor(): ContractDescriptor

  def buildOracleInfo(): OracleInfo

  def buildContractInfo(
      totalCollateral: Satoshis,
      contractDescriptor: ContractDescriptor,
      oracleInfo: OracleInfo): ContractInfo = {
    ContractInfo(
      totalCollateral,
      ContractOraclePair.fromDescriptorOracle(contractDescriptor, oracleInfo))
  }

  def buildOffer(
      contractInfo: ContractInfo,
      publicKeys: DLCPublicKeys,
      offerCollateral: Satoshis,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): DLCOffer = {
    DLCOffer(contractInfo,
             publicKeys,
             offerCollateral,
             fundingInputs,
             changeAddress,
             feeRate,
             timeouts)
  }

  // TODO: Split up, de-futurify
  def buildFundingTransaction(
      offerFundingKey: ECPublicKey,
      acceptFundingKey: ECPublicKey,
      feeRate: SatoshisPerVirtualByte,
      offerInput: CurrencyUnit,
      acceptInput: CurrencyUnit,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      offerChangeSPK: ScriptPubKey,
      acceptChangeSPK: ScriptPubKey,
      offerPayoutSPK: ScriptPubKey,
      acceptPayoutSPK: ScriptPubKey)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    // The total collateral of both parties combined
    val totalInput: CurrencyUnit = offerInput + acceptInput

    // The sum of all funding input amounts from the initiator
    val offerTotalFunding: CurrencyUnit =
      offerFundingInputs.map(_.output.value).sum

    // The sum of all funding input amounts from the non-initiator
    val acceptTotalFunding: CurrencyUnit =
      acceptFundingInputs.map(_.output.value).sum

    require(
      offerTotalFunding >= offerInput,
      "Offer funding inputs must add up to at least offer's total collateral")
    require(
      acceptTotalFunding >= acceptInput,
      "Accept funding inputs must add up to at least accept's total collateral")

    val fundingKeys =
      Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

    // The 2-of-2 MultiSignatureScriptPubKey to be wrapped in P2WSH and used as the funding output
    val fundingMultiSig: MultiSignatureScriptPubKey =
      MultiSignatureScriptPubKey(2, fundingKeys)

    // The funding output's P2WSH(MultiSig) ScriptPubKey
    val fundingSPK: P2WSHWitnessSPKV0 = P2WSHWitnessSPKV0(fundingMultiSig)

    val fundingTxFinalizer: DualFundingTxFinalizer = DualFundingTxFinalizer(
      offerInputs = offerFundingInputs.map(_.toDualFundingInput),
      offerPayoutSPK = offerPayoutSPK,
      offerChangeSPK = offerChangeSPK,
      acceptInputs = acceptFundingInputs.map(_.toDualFundingInput),
      acceptPayoutSPK = acceptPayoutSPK,
      acceptChangeSPK = acceptChangeSPK,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )
    val builder = RawTxBuilderWithFinalizer(fundingTxFinalizer)

    builder += TransactionOutput(totalInput, fundingSPK)
    builder += TransactionOutput(offerTotalFunding - offerInput, offerChangeSPK)
    builder += TransactionOutput(acceptTotalFunding - acceptInput,
                                 acceptChangeSPK)

    offerFundingInputs.foreach { ref =>
      val scriptSig = ref.redeemScriptOpt match {
        case Some(redeemScript) => P2SHScriptSignature(redeemScript)
        case None               => EmptyScriptSignature
      }

      builder += TransactionInput(ref.outPoint,
                                  scriptSig,
                                  TransactionConstants.sequence)
    }

    acceptFundingInputs.foreach { ref =>
      val scriptSig = ref.redeemScriptOpt match {
        case Some(redeemScript) => P2SHScriptSignature(redeemScript)
        case None               => EmptyScriptSignature
      }

      builder += TransactionInput(ref.outPoint,
                                  scriptSig,
                                  TransactionConstants.sequence)
    }

    builder.buildTx()
  }

  def buildCET(
      outcome: OracleOutcome,
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      timeouts: DLCTimeouts,
      fundingOutputRef: OutputReference): WitnessTransaction = {
    val builder =
      DLCCETBuilder(contractInfo,
                    offerFundingKey,
                    offerFinalSPK,
                    acceptFundingKey,
                    acceptFinalSPK,
                    timeouts,
                    fundingOutputRef)

    builder.buildCET(outcome)
  }

  def buildCETs(
      outcomes: Vector[OracleOutcome],
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      timeouts: DLCTimeouts,
      fundingOutputRef: OutputReference): Vector[
    (OracleOutcome, WitnessTransaction)] = {
    val builder =
      DLCCETBuilder(contractInfo,
                    offerFundingKey,
                    offerFinalSPK,
                    acceptFundingKey,
                    acceptFinalSPK,
                    timeouts,
                    fundingOutputRef)

    outcomes.map { outcome =>
      (outcome, builder.buildCET(outcome))
    }
  }

  def adaptorSignCET(
      outcome: OracleOutcome,
      fundingKey: ECPrivateKey): ECAdaptorSignature = {
    val cet: WitnessTransaction = ???
    val cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo] = ???
    val adaptorPoint = outcome.sigPoint
    val hashToSign =
      TransactionSignatureSerializer.hashForSignature(cet,
                                                      cetSigningInfo,
                                                      HashType.sigHashAll)

    fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
  }

  def adaptorSignCETs(
      outcomes: OracleOutcome,
      fundingKey: ECPrivateKey): Vector[(OracleOutcome, ECAdaptorSignature)] = {
    val outcomesAndCETs: Vector[(OracleOutcome, WitnessTransaction)] = ???
    val cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo] = ???

    outcomesAndCETs.map { case (outcome, cet) =>
      val adaptorPoint = outcome.sigPoint
      val hashToSign =
        TransactionSignatureSerializer.hashForSignature(cet,
                                                        cetSigningInfo,
                                                        HashType.sigHashAll)

      val adaptorSig = fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
      (outcome, adaptorSig)
    }
  }

  // TODO: clean up
  def buildRefundTx(
      offerInput: CurrencyUnit,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      acceptInput: CurrencyUnit,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      fundingOutputRef: OutputReference,
      timeouts: DLCTimeouts): WitnessTransaction = {
    val OutputReference(fundingOutPoint, fundingOutput) = fundingOutputRef
    val fundingKeys = Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)
    val fundingInfo = P2WSHV0InputInfo(
      outPoint = fundingOutPoint,
      amount = fundingOutput.value,
      scriptWitness =
        P2WSHWitnessV0(MultiSignatureScriptPubKey(2, fundingKeys)),
      conditionalPath = ConditionalPath.NoCondition
    )

    WitnessTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(fundingOutPoint,
                         EmptyScriptSignature,
                         TransactionConstants.disableRBFSequence)),
      Vector(TransactionOutput(offerInput, offerFinalSPK),
             TransactionOutput(acceptInput, acceptFinalSPK)),
      timeouts.contractTimeout.toUInt32,
      TransactionWitness.fromWitOpt(
        Vector(InputInfo.getScriptWitness(fundingInfo)))
    )
  }

  // TODO: do this directly without touching PSBTs
  def signRefundTx(): PartialSignature

  def buildAccept(
      acceptCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures,
      negotiationFields: DLCAccept.NegotiationFields,
      tempContractId: Sha256Digest): DLCAccept = {
    DLCAccept(acceptCollateral,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              negotiationFields,
              tempContractId)
  }

  // TODO: add method for Vector[(OracleOutcome, ECAdaptorSignature)] once implemented
  def validateCETSignature(
      outcome: OracleOutcome,
      sig: ECAdaptorSignature): Boolean = {
    val fundingTx: Transaction = ???
    val remoteFundingPubKey: ECPublicKey = ???
    val adaptorPoint = outcome.sigPoint
    val cet: WitnessTransaction = ???

    val sigComponent = WitnessTxSigComponentRaw(transaction = cet,
                                                inputIndex = UInt32.zero,
                                                output = fundingTx.outputs.head,
                                                flags = Policy.standardFlags)

    val hashType = HashType(
      ByteVector(0.toByte, 0.toByte, 0.toByte, HashType.sigHashAll.byte))
    val hash =
      TransactionSignatureSerializer.hashForSignature(sigComponent, hashType)

    remoteFundingPubKey.adaptorVerify(hash.bytes, adaptorPoint, sig)
  }

  def validateRefundTx(refundSig: PartialSignature): Boolean = {
    val fundingTx: Transaction = ???
    val refundTx: WitnessTransaction = ???

    val sigComponent = WitnessTxSigComponentRaw(transaction = refundTx,
                                                inputIndex = UInt32.zero,
                                                output = fundingTx.outputs.head,
                                                flags = Policy.standardFlags)

    TransactionSignatureChecker
      .checkSignature(
        sigComponent,
        sigComponent.output.scriptPubKey.asm.toVector,
        refundSig.pubKey,
        refundSig.signature,
        Policy.standardFlags
      )
      .isValid
  }

  def buildSign(): DLCSign

  def validateFundingSigs(): Boolean

  def computeOutcome(
      oracleSigs: Vector[OracleSignatures],
      contractInfo: ContractInfo): Option[OracleOutcome] = {
    contractInfo.findOutcome(oracleSigs)
  }

  def completeCET(): WitnessTransaction

  def executeDLC(): ExecutedDLCOutcome

  def executeRefund(): RefundDLCOutcome

  def computeOutcome(completedLocalSig: ECDigitalSignature): (
      SchnorrDigitalSignature,
      OracleOutcome)
}
