package org.bitcoins.dlc.sign

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.TransactionSignatureSerializer
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

/** Responsible for constructing all DLC signatures
  * and signed transactions
  */
case class DLCTxSigner(
    builder: DLCTxBuilder,
    isInitiator: Boolean,
    fundingKey: ECPrivateKey,
    finalAddress: BitcoinAddress,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
    ec: ExecutionContext) {

  private val offer = builder.offer
  private val accept = builder.accept

  private val remoteFundingPubKey = if (isInitiator) {
    accept.pubKeys.fundingKey
  } else {
    offer.pubKeys.fundingKey
  }

  if (isInitiator) {
    require(fundingKey.publicKey == offer.pubKeys.fundingKey &&
              finalAddress == offer.pubKeys.payoutAddress,
            "Given keys do not match public key and address in offer")
    require(fundingUtxos.map(
              DLCFundingInput.fromInputSigningInfo(_)) == offer.fundingInputs,
            "Funding ScriptSignatureParams did not match offer funding inputs")
  } else {
    require(
      fundingKey.publicKey == accept.pubKeys.fundingKey &&
        finalAddress == accept.pubKeys.payoutAddress,
      "Given keys do not match public key and address in accept"
    )
    require(fundingUtxos.map(
              DLCFundingInput.fromInputSigningInfo(_)) == accept.fundingInputs,
            "Funding ScriptSignatureParams did not match accept funding inputs")
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sigs: Vector[OracleSignatures]): CurrencyUnit = {
    val (offerPayout, acceptPayout) = builder.getPayouts(sigs)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  /** Creates this party's FundingSignatures */
  def createFundingTxSigs(): Future[FundingSignatures] = {
    builder.buildFundingTx.flatMap { fundingTx =>
      DLCTxSigner.signFundingTx(fundingTx, fundingUtxos)
    }
  }

  /** Constructs the signed DLC funding transaction given remote FundingSignatures */
  def signFundingTx(remoteSigs: FundingSignatures): Future[Transaction] = {
    for {
      localSigs <- createFundingTxSigs()
      fundingTx <- builder.buildFundingTx
      signedTxT = DLCTxSigner.completeFundingTx(localSigs,
                                                remoteSigs,
                                                offer.fundingInputs,
                                                accept.fundingInputs,
                                                fundingTx)
      signedTx <- Future.fromTry(signedTxT)
    } yield signedTx
  }

  private var _cetSigningInfo: Option[ECSignatureParams[P2WSHV0InputInfo]] =
    None

  private def cetSigningInfo: Future[ECSignatureParams[P2WSHV0InputInfo]] = {
    _cetSigningInfo match {
      case Some(info) => Future.successful(info)
      case None =>
        builder.buildFundingTx.map { fundingTx =>
          val signingInfo =
            DLCTxSigner.buildCETSigningInfo(fundingTx,
                                            builder.fundingMultiSig,
                                            fundingKey)

          _cetSigningInfo = Some(signingInfo)

          signingInfo
        }
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcome */
  def createRemoteCETSig(outcome: OracleOutcome): Future[ECAdaptorSignature] = {
    createRemoteCETSigs(Vector(outcome)).map(_.head._2)
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def createRemoteCETsAndSigs(outcomes: Vector[OracleOutcome]): Future[
    Vector[(OracleOutcome, WitnessTransaction, ECAdaptorSignature)]] = {
    for {
      outcomesAndCETs <- builder.buildCETsMap(outcomes)
      signingInfo <- cetSigningInfo
    } yield {
      DLCTxSigner.buildAndSignCETs(outcomesAndCETs, signingInfo, fundingKey)
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def createRemoteCETSigs(outcomes: Vector[OracleOutcome]): Future[
    Vector[(OracleOutcome, ECAdaptorSignature)]] = {
    createRemoteCETsAndSigs(outcomes).map(_.map { case (outcome, _, sig) =>
      outcome -> sig
    })
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes and their corresponding CETs */
  def createRemoteSigsGivenCETs(
      outcomesAndCETs: Vector[(OracleOutcome, WitnessTransaction)]): Future[
    Vector[(OracleOutcome, ECAdaptorSignature)]] = {
    cetSigningInfo.map { signingInfo =>
      DLCTxSigner.signCETs(outcomesAndCETs, signingInfo, fundingKey)
    }
  }

  def signCET(
      outcome: OracleOutcome,
      remoteAdaptorSig: ECAdaptorSignature,
      oracleSigs: Vector[OracleSignatures]): Future[Transaction] = {
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildCET(outcome)
      cet <- DLCTxSigner.completeCET(outcome,
                                     fundingKey,
                                     builder.fundingMultiSig,
                                     fundingTx,
                                     utx,
                                     remoteAdaptorSig,
                                     remoteFundingPubKey,
                                     oracleSigs)
    } yield cet
  }

  /** Creates this party's signature of the refund transaction */
  def createRefundSig(): Future[PartialSignature] = {
    for {
      fundingTx <- builder.buildFundingTx
      refundTx <- builder.buildRefundTx
      refundSig <- DLCTxSigner.signRefundTx(fundingKey,
                                            remoteFundingPubKey,
                                            fundingTx,
                                            refundTx)
    } yield refundSig
  }

  /** Constructs the signed refund transaction given remote's signature */
  def signRefundTx(remoteSig: PartialSignature): Future[Transaction] = {
    for {
      localSig <- createRefundSig()
      fundingTx <- builder.buildFundingTx
      uRefundTx <- builder.buildRefundTx
      refundTxT = DLCTxSigner.completeRefundTx(localSig,
                                               remoteSig,
                                               builder.fundingMultiSig,
                                               fundingTx,
                                               uRefundTx)
      refundTx <- Future.fromTry(refundTxT)
    } yield refundTx
  }

  /** Creates all of this party's CETSignatures */
  def createCETSigs(): Future[CETSignatures] = {
    for {
      cetSigs <- createRemoteCETSigs(builder.contractInfo.allOutcomes)
      refundSig <- createRefundSig()
    } yield CETSignatures(cetSigs, refundSig)
  }

  /** Creates all of this party's CETSignatures */
  def createCETsAndSigs(): Future[
    (CETSignatures, Vector[WitnessTransaction])] = {
    for {
      cetsAndSigs <- createRemoteCETsAndSigs(builder.contractInfo.allOutcomes)
      refundSig <- createRefundSig()
    } yield {
      val (msgs, cets, sigs) = cetsAndSigs.unzip3
      (CETSignatures(msgs.zip(sigs), refundSig), cets)
    }
  }

  /** Creates this party's CETSignatures given the outcomes and their unsigned CETs */
  def createCETSigs(
      outcomesAndCETs: Vector[(OracleOutcome, WitnessTransaction)]): Future[
    CETSignatures] = {
    for {
      cetSigs <- createRemoteSigsGivenCETs(outcomesAndCETs)
      refundSig <- createRefundSig()
    } yield CETSignatures(cetSigs, refundSig)
  }
}

object DLCTxSigner {

  def apply(
      builder: DLCTxBuilder,
      isInitiator: Boolean,
      fundingKey: ECPrivateKey,
      payoutPrivKey: ECPrivateKey,
      network: BitcoinNetwork,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
      ec: ExecutionContext): DLCTxSigner = {
    val payoutAddr =
      Bech32Address(P2WPKHWitnessSPKV0(payoutPrivKey.publicKey), network)
    DLCTxSigner(builder, isInitiator, fundingKey, payoutAddr, fundingUtxos)
  }

  def buildCETSigningInfo(
      fundingTx: Transaction,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingKey: ECPrivateKey): ECSignatureParams[P2WSHV0InputInfo] = {
    val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)

    ECSignatureParams(
      P2WSHV0InputInfo(
        outPoint = fundingOutPoint,
        amount = fundingTx.outputs.head.value,
        scriptWitness = P2WSHWitnessV0(fundingMultiSig),
        conditionalPath = ConditionalPath.NoCondition
      ),
      fundingTx,
      fundingKey,
      HashType.sigHashAll
    )
  }

  def signCET(
      outcome: OracleOutcome,
      cet: WitnessTransaction, // TODO: compute in overload
      cetSigningInfo: ECSignatureParams[
        P2WSHV0InputInfo
      ], // TODO: compute in overload
      fundingKey: ECPrivateKey): ECAdaptorSignature = {
    signCETs(Vector((outcome, cet)), cetSigningInfo, fundingKey).head._2
  }

  def signCETs(
      outcomesAndCETs: Vector[
        (OracleOutcome, WitnessTransaction)
      ], // TODO: compute in overload
      cetSigningInfo: ECSignatureParams[
        P2WSHV0InputInfo
      ], // TODO: compute in overload
      fundingKey: ECPrivateKey): Vector[(OracleOutcome, ECAdaptorSignature)] = {
    buildAndSignCETs(outcomesAndCETs, cetSigningInfo, fundingKey).map {
      case (outcome, _, sig) => outcome -> sig
    }
  }

  def buildAndSignCETs(
      outcomesAndCETs: Vector[
        (OracleOutcome, WitnessTransaction)
      ], // TODO: compute in overload
      cetSigningInfo: ECSignatureParams[
        P2WSHV0InputInfo
      ], // TODO: compute in overload
      fundingKey: ECPrivateKey): Vector[
    (OracleOutcome, WitnessTransaction, ECAdaptorSignature)] = {
    outcomesAndCETs.map { case (outcome, cet) =>
      val adaptorPoint = outcome.sigPoint
      val hashToSign =
        TransactionSignatureSerializer.hashForSignature(cet,
                                                        cetSigningInfo,
                                                        HashType.sigHashAll)

      val adaptorSig = fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
      (outcome, cet, adaptorSig)
    }
  }

  // TODO: Without PSBTs
  def completeCET(
      outcome: OracleOutcome,
      fundingKey: ECPrivateKey,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingTx: Transaction, // TODO: compute in overload
      ucet: WitnessTransaction, // TODO: compute in overload
      remoteAdaptorSig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      oracleSigs: Vector[OracleSignatures])(implicit
      ec: ExecutionContext): Future[WitnessTransaction] = {
    val oracleSigSum =
      OracleSignatures.computeAggregateSignature(outcome, oracleSigs)
    val remoteSig =
      oracleSigSum
        .completeAdaptorSignature(remoteAdaptorSig, HashType.sigHashAll.byte)

    val remotePartialSig = PartialSignature(remoteFundingPubKey, remoteSig)
    for {
      psbt <-
        PSBT
          .fromUnsignedTx(ucet)
          .addUTXOToInput(fundingTx, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingMultiSig), index = 0)
          .addSignature(remotePartialSig, inputIndex = 0)
          .sign(inputIndex = 0, fundingKey)

      cetT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      cet <- Future.fromTry(cetT)
    } yield {
      cet.asInstanceOf[WitnessTransaction]
    }
  }

  // TODO: do this directly without touching PSBTs
  def signRefundTx(
      fundingKey: ECPrivateKey,
      remoteFundingKey: ECPublicKey,
      fundingTx: Transaction, // TODO: compute in overload
      refundTx: WitnessTransaction // TODO: compute in overload
  )(implicit ec: ExecutionContext): Future[PartialSignature] = {
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

  def completeRefundTx(
      localSig: PartialSignature,
      remoteSig: PartialSignature,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingTx: Transaction,
      uRefundTx: WitnessTransaction): Try[WitnessTransaction] = {
    val psbt = PSBT
      .fromUnsignedTx(uRefundTx)
      .addUTXOToInput(fundingTx, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingMultiSig), index = 0)
      .addSignature(localSig, inputIndex = 0)
      .addSignature(remoteSig, inputIndex = 0)

    psbt.finalizePSBT
      .flatMap(_.extractTransactionAndValidate)
      .map(_.asInstanceOf[WitnessTransaction])
  }

  def signFundingTx(
      fundingTx: Transaction, // TODO: compute in overload
      fundingUtxos: Vector[
        ScriptSignatureParams[InputInfo]
      ] // TODO: compute in overload
  )(implicit ec: ExecutionContext): Future[FundingSignatures] = {
    val sigFs =
      Vector.newBuilder[Future[(TransactionOutPoint, ScriptWitnessV0)]]

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

  def completeFundingTx(
      localSigs: FundingSignatures,
      remoteSigs: FundingSignatures,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      fundingTx: Transaction): Try[Transaction] = {
    val fundingInputs = offerFundingInputs ++ acceptFundingInputs
    val allSigs = localSigs.merge(remoteSigs)

    val psbt = fundingInputs.zipWithIndex.foldLeft(
      PSBT.fromUnsignedTxWithP2SHScript(fundingTx)) {
      case (psbt, (fundingInput, index)) =>
        val witness = allSigs(fundingInput.outPoint)

        psbt
          .addUTXOToInput(fundingInput.prevTx, index)
          .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                            witness,
                                            index)
    }

    val finalizedT = if (psbt.isFinalized) {
      Success(psbt)
    } else {
      psbt.finalizePSBT
    }

    finalizedT.flatMap(_.extractTransactionAndValidate)
  }
}
