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
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.{
  DualFundingTxFinalizer,
  RawTxBuilderWithFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCCETBuilder
import org.bitcoins.dlc.execution.{ExecutedDLCOutcome, RefundDLCOutcome}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object DLC extends BitcoinSLogger {

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
  def signRefundTx(fundingKey: ECPrivateKey, remoteFundingKey: ECPublicKey)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    val fundingTx: Transaction = ???
    val refundTx: WitnessTransaction = ???
    val fundingKeys =
      Vector(fundingKey.publicKey, remoteFundingKey).sortBy(_.hex)
    val fundingSPK = MultiSignatureScriptPubKey(2, fundingKeys)

    val psbtF = PSBT
      .fromUnsignedTx(refundTx)
      .addUTXOToInput(fundingTx, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      .sign(inputIndex = 0, fundingKey)

    psbtF.flatMap { psbt =>
      val sigOpt = psbt.inputMaps.head.partialSignatures
        .find(_.pubKey == fundingKey.publicKey)

      sigOpt match {
        case None =>
          Future.failed(
            new RuntimeException("No signature found after signing"))
        case Some(partialSig) => Future.successful(partialSig)
      }
    }
  }

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

  def signFundingTx()(implicit
      ec: ExecutionContext): Future[FundingSignatures] = {
    val sigFs =
      Vector.newBuilder[Future[(TransactionOutPoint, ScriptWitnessV0)]]

    val fundingTx: Transaction = ???

    val fundingUtxos: Vector[ScriptSignatureParams[InputInfo]] = ???
    val fundingInputs: Vector[DLCFundingInput] =
      fundingUtxos.map(DLCFundingInput.fromInputSigningInfo(_))

    fundingUtxos.foreach { utxo =>
      val sigComponentF =
        BitcoinSigner.sign(utxo, fundingTx, isDummySignature = false)
      val witnessF = sigComponentF.flatMap { sigComponent =>
        sigComponent.transaction match {
          case wtx: WitnessTransaction =>
            val witness = wtx.witness(sigComponent.inputIndex.toInt)
            if (witness == EmptyScriptWitness) {
              Future.failed(
                new RuntimeException(s"Funding Inputs must be SegWit: $utxo"))
            } else {
              Future.successful(witness)
            }
          case _: NonWitnessTransaction =>
            Future.failed(
              new RuntimeException(s"Funding Inputs must be SegWit: $utxo"))
        }
      }

      sigFs += witnessF.flatMap {
        case witness: ScriptWitnessV0 =>
          Future.successful((utxo.outPoint, witness))
        case witness: ScriptWitness =>
          Future.failed(
            new RuntimeException(s"Unrecognized script witness: $witness"))
      }
    }

    val sigsF = Future.sequence(sigFs.result())

    sigsF.map { sigs =>
      val sigsMap = sigs.toMap

      val sigsVec = fundingInputs.map { input =>
        input.outPoint -> sigsMap(input.outPoint)
      }

      FundingSignatures(sigsVec)
    }
  }

  def buildSign(contractId: ByteVector): DLCSign = {
    val cetSigs: CETSignatures = ???
    val fundingSigs: FundingSignatures = ???

    DLCSign(cetSigs, fundingSigs, contractId)
  }

  // TODO: Don't use PSBT
  def validateFundingSigs(
      fundingSigs: FundingSignatures,
      isInitiator: Boolean,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput]): Boolean = {
    val fundingTx = ???

    val (remoteTweak, remoteFundingInputs) = if (isInitiator) {
      (offerFundingInputs.length, acceptFundingInputs)
    } else {
      (0, offerFundingInputs)
    }

    val psbt = PSBT.fromUnsignedTxWithP2SHScript(fundingTx)

    fundingSigs.zipWithIndex
      .foldLeft(true) { case (ret, ((outPoint, witness), index)) =>
        val idx = index + remoteTweak
        if (ret) {
          if (psbt.transaction.inputs(idx).previousOutput != outPoint) {
            logger.error("Adding signature for incorrect input")

            false
          } else {
            val fundingInput = remoteFundingInputs(index)

            psbt
              .addUTXOToInput(fundingInput.prevTx, idx)
              .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                                witness,
                                                idx)
              .finalizeInput(idx) match {
              case Success(finalized) =>
                finalized.verifyFinalizedInput(idx)
              case Failure(_) =>
                false
            }
          }
        } else false
      }
  }

  def computeOutcome(
      oracleSigs: Vector[OracleSignatures],
      contractInfo: ContractInfo): Option[OracleOutcome] = {
    contractInfo.findOutcome(oracleSigs)
  }

  // TODO: Without PSBTs
  def completeCET(
      outcome: OracleOutcome,
      fundingKey: ECPrivateKey,
      remoteAdaptorSig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      oracleSigs: Vector[OracleSignatures])(implicit
      ec: ExecutionContext): Future[WitnessTransaction] = {
    val fundingTx: Transaction = ???
    val utx: WitnessTransaction = ???

    val fundingKeys =
      Vector(fundingKey.publicKey, remoteFundingPubKey).sortBy(_.hex)
    val fundingSPK = MultiSignatureScriptPubKey(2, fundingKeys)

    val oracleSigSum =
      OracleSignatures.computeAggregateSignature(outcome, oracleSigs)
    val remoteSig =
      oracleSigSum
        .completeAdaptorSignature(remoteAdaptorSig, HashType.sigHashAll.byte)

    val remotePartialSig = PartialSignature(remoteFundingPubKey, remoteSig)
    for {
      psbt <-
        PSBT
          .fromUnsignedTx(utx)
          .addUTXOToInput(fundingTx, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
          .addSignature(remotePartialSig, inputIndex = 0)
          .sign(inputIndex = 0, fundingKey)

      cetT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      cet <- Future.fromTry(cetT)
    } yield {
      cet.asInstanceOf[WitnessTransaction]
    }
  }

  def executeDLC(
      remoteAdaptorSigs: Vector[(OracleOutcome, ECAdaptorSignature)],
      oracleSigs: Vector[OracleSignatures],
      fundingKey: ECPrivateKey,
      remoteFundingPubKey: ECPublicKey)(implicit
      ec: ExecutionContext): Future[ExecutedDLCOutcome] = {
    val contractInfo: ContractInfo = ???
    val fundingTx: Transaction = ???
    val threshold = contractInfo.oracleInfo.threshold
    val sigCombinations = CETCalculator.combinations(oracleSigs, threshold)

    var msgOpt: Option[OracleOutcome] = None
    val sigsUsedOpt = sigCombinations.find { sigs =>
      msgOpt = contractInfo.findOutcome(sigs)
      msgOpt.isDefined
    }

    val msgAndSigOpt = msgOpt.flatMap { msg =>
      remoteAdaptorSigs.find(_._1 == msg)
    }

    val (msg, remoteAdaptorSig) = msgAndSigOpt match {
      case Some(msgAndSig) => msgAndSig
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to any possible outcome! $oracleSigs")
    }
    val sigsUsed = sigsUsedOpt.get // Safe because msgOpt is defined if no throw

    val cetF = completeCET(msg,
                           fundingKey,
                           remoteAdaptorSig,
                           remoteFundingPubKey,
                           sigsUsed)

    cetF.map { cet =>
      ExecutedDLCOutcome(fundingTx, cet, msg, sigsUsed)
    }
  }

  def executeRefund(): RefundDLCOutcome = {
    val fundingTx: Transaction = ???
    val refundTx: WitnessTransaction = ???

    RefundDLCOutcome(fundingTx, refundTx)
  }

  /** Extracts an adaptor secret from cetSig assuming it is the completion
    * adaptorSig (which it may not be) and returns the oracle signature if
    * and only if adaptorSig does correspond to cetSig.
    *
    * This method is used to search through possible cetSigs until the correct
    * one is found by validating the returned signature.
    *
    * @param outcome A potential outcome that could have been executed for
    * @param adaptorSig The adaptor signature corresponding to outcome
    * @param cetSig The actual signature for local's key found on-chain on a CET
    */
  private def sigFromOutcomeAndSigs(
      outcome: OracleOutcome,
      adaptorSig: ECAdaptorSignature,
      cetSig: ECDigitalSignature): SchnorrDigitalSignature = {
    val sigPubKey = outcome.sigPoint

    // This value is either the oracle signature S value or it is
    // useless garbage, but we don't know in this scope, the caller
    // must do further work to check this.
    val possibleOracleS =
      sigPubKey
        .extractAdaptorSecret(adaptorSig, ECDigitalSignature(cetSig.bytes.init))
        .fieldElement

    SchnorrDigitalSignature(outcome.aggregateNonce, possibleOracleS)
  }

  def computeOutcome(
      completedSig: ECDigitalSignature,
      possibleAdaptorSigs: Vector[(OracleOutcome, ECAdaptorSignature)]): (
      SchnorrDigitalSignature,
      OracleOutcome) = {
    val sigOpt = possibleAdaptorSigs.find { case (outcome, adaptorSig) =>
      val possibleOracleSig =
        sigFromOutcomeAndSigs(outcome, adaptorSig, completedSig)
      possibleOracleSig.sig.getPublicKey == outcome.sigPoint
    }

    sigOpt match {
      case Some((outcome, adaptorSig)) =>
        (sigFromOutcomeAndSigs(outcome, adaptorSig, completedSig), outcome)
      case None =>
        throw new IllegalArgumentException("No Oracle Signature found from CET")
    }
  }

  def computeOutcome(
      isInitiator: Boolean,
      offerFundingKey: ECPublicKey,
      acceptFundingKey: ECPublicKey,
      contractInfo: ContractInfo,
      localAdaptorSigs: Vector[(OracleOutcome, ECAdaptorSignature)],
      cet: Transaction): (SchnorrDigitalSignature, OracleOutcome) = {
    val wCET = cet match {
      case wtx: WitnessTransaction => wtx
      case _: NonWitnessTransaction =>
        throw new IllegalArgumentException(s"Expected Witness CET: $cet")
    }

    val cetSigs = wCET.witness.head
      .asInstanceOf[P2WSHWitnessV0]
      .signatures

    require(cetSigs.size == 2,
            s"There must be only 2 signatures, got ${cetSigs.size}")

    val outcomeValues = wCET.outputs.map(_.value).sorted
    val totalCollateral = contractInfo.totalCollateral

    val possibleOutcomes = contractInfo.allOutcomesAndPayouts
      .filter { case (_, amt) =>
        val amts = Vector(amt, totalCollateral - amt)
          .filter(_ >= Policy.dustThreshold)
          .sorted

        // Only messages within 1 satoshi of the on-chain CET's value
        // should be considered.
        // Off-by-one is okay because both parties round to the nearest
        // Satoshi for fees and if both round up they could be off-by-one.
        Math.abs((amts.head - outcomeValues.head).satoshis.toLong) <= 1 && Math
          .abs((amts.last - outcomeValues.last).satoshis.toLong) <= 1
      }
      .map(_._1)

    val (offerCETSig, acceptCETSig) =
      if (offerFundingKey.hex.compareTo(acceptFundingKey.hex) > 0) {
        (cetSigs.last, cetSigs.head)
      } else {
        (cetSigs.head, cetSigs.last)
      }

    val outcomeSigs = localAdaptorSigs.filter { case (outcome, _) =>
      possibleOutcomes.contains(outcome)
    }

    val cetSig = if (isInitiator) {
      acceptCETSig
    } else {
      offerCETSig
    }

    computeOutcome(cetSig, outcomeSigs)
  }
}
