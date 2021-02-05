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
import scala.util.Success

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

  private val fundingSPK: MultiSignatureScriptPubKey =
    builder.fundingTxBuilder.fundingMultiSig

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
    val sigFs =
      Vector.newBuilder[Future[(TransactionOutPoint, ScriptWitnessV0)]]

    for {
      fundingTx <- builder.buildFundingTx

      _ = {
        fundingUtxos.foreach { utxo =>
          val sigComponentF =
            BitcoinSigner.sign(utxo, fundingTx, isDummySignature = false)
          val witnessF = sigComponentF.flatMap { sigComponent =>
            sigComponent.transaction match {
              case wtx: WitnessTransaction =>
                val witness = wtx.witness(sigComponent.inputIndex.toInt)
                if (witness == EmptyScriptWitness) {
                  Future.failed(
                    new RuntimeException(
                      s"Funding Inputs must be SegWit: $utxo"))
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
      }

      sigs <- Future.sequence(sigFs.result())
    } yield {
      val sigsMap = sigs.toMap

      val fundingInputs = if (isInitiator) {
        offer.fundingInputs
      } else {
        accept.fundingInputs
      }

      val sigsVec = fundingInputs.map { input =>
        input.outPoint -> sigsMap(input.outPoint)
      }

      FundingSignatures(sigsVec)
    }
  }

  /** Constructs the signed DLC funding transaction given remote FundingSignatures */
  def signFundingTx(remoteSigs: FundingSignatures): Future[Transaction] = {
    val fundingInputs = offer.fundingInputs ++ accept.fundingInputs

    val psbtF = for {
      localSigs <- createFundingTxSigs()
      allSigs = localSigs.merge(remoteSigs)
      fundingTx <- builder.buildFundingTx
    } yield {
      fundingInputs.zipWithIndex.foldLeft(
        PSBT.fromUnsignedTxWithP2SHScript(fundingTx)) {
        case (psbt, (fundingInput, index)) =>
          val witness = allSigs(fundingInput.outPoint)

          psbt
            .addUTXOToInput(fundingInput.prevTx, index)
            .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                              witness,
                                              index)
      }
    }

    psbtF.flatMap { psbt =>
      val finalizedT = if (psbt.isFinalized) {
        Success(psbt)
      } else {
        psbt.finalizePSBT
      }

      val txT = finalizedT.flatMap(_.extractTransactionAndValidate)
      Future.fromTry(txT)
    }
  }

  private def findSigInPSBT(
      psbt: PSBT,
      pubKey: ECPublicKey): Future[PartialSignature] = {
    val sigOpt = psbt.inputMaps.head.partialSignatures
      .find(_.pubKey == pubKey)

    sigOpt match {
      case None =>
        Future.failed(new RuntimeException("No signature found after signing"))
      case Some(partialSig) => Future.successful(partialSig)
    }
  }

  private var _cetSigningInfo: Option[ECSignatureParams[P2WSHV0InputInfo]] =
    None

  private def cetSigningInfo: Future[ECSignatureParams[P2WSHV0InputInfo]] = {
    _cetSigningInfo match {
      case Some(info) => Future.successful(info)
      case None =>
        for {
          fundingTx <- builder.buildFundingTx
          fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
        } yield {
          val signingInfo = ECSignatureParams(
            P2WSHV0InputInfo(outPoint = fundingOutPoint,
                             amount = fundingTx.outputs.head.value,
                             scriptWitness = P2WSHWitnessV0(fundingSPK),
                             conditionalPath = ConditionalPath.NoCondition),
            fundingTx,
            fundingKey,
            HashType.sigHashAll
          )

          _cetSigningInfo = Some(signingInfo)

          signingInfo
        }
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcome */
  def createRemoteCETSig(outcome: OracleOutcome): Future[ECAdaptorSignature] = {
    val adaptorPoint = outcome.sigPoint
    for {
      utx <- builder.buildCET(outcome)
      signingInfo <- cetSigningInfo
    } yield {
      val hashToSign = TransactionSignatureSerializer.hashForSignature(
        utx,
        signingInfo,
        HashType.sigHashAll)

      fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def createRemoteCETsAndSigs(outcomes: Vector[OracleOutcome]): Future[
    Vector[(OracleOutcome, WitnessTransaction, ECAdaptorSignature)]] = {
    for {
      cetBuilder <- builder.cetBuilderF
      signingInfo <- cetSigningInfo
    } yield {
      outcomes.map { outcome =>
        val adaptorPoint = outcome.sigPoint
        val utx = cetBuilder.buildCET(outcome)
        val hashToSign =
          TransactionSignatureSerializer.hashForSignature(utx,
                                                          signingInfo,
                                                          HashType.sigHashAll)

        (outcome, utx, fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes))
      }
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
    for {
      signingInfo <- cetSigningInfo
    } yield {
      outcomesAndCETs.map { case (outcome, utx) =>
        val adaptorPoint = outcome.sigPoint
        val hashToSign =
          TransactionSignatureSerializer.hashForSignature(utx,
                                                          signingInfo,
                                                          HashType.sigHashAll)

        outcome -> fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
      }
    }
  }

  def signCET(
      outcome: OracleOutcome,
      remoteAdaptorSig: ECAdaptorSignature,
      oracleSigs: Vector[OracleSignatures]): Future[Transaction] = {
    val oracleSigSum =
      OracleSignatures.computeAggregateSignature(outcome, oracleSigs)
    val remoteSig =
      oracleSigSum
        .completeAdaptorSignature(remoteAdaptorSig, HashType.sigHashAll.byte)

    val remotePartialSig = PartialSignature(remoteFundingPubKey, remoteSig)
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildCET(outcome)

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
      cet
    }
  }

  /** Creates a PSBT of the refund transaction which contain's this
    * party's signature
    */
  def createPartiallySignedRefundTx(): Future[PSBT] = {
    for {
      fundingTx <- builder.buildFundingTx
      utx <- builder.buildRefundTx
      psbt <-
        PSBT
          .fromUnsignedTx(utx)
          .addUTXOToInput(fundingTx, index = 0)
          .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
          .sign(inputIndex = 0, fundingKey)
    } yield {
      psbt
    }
  }

  /** Creates this party's signature of the refund transaction */
  def createRefundSig(): Future[PartialSignature] = {
    for {
      psbt <- createPartiallySignedRefundTx()
      signature <- findSigInPSBT(psbt, fundingKey.publicKey)
    } yield {
      signature
    }
  }

  /** Constructs the signed refund transaction given remote's signature */
  def signRefundTx(remoteSig: PartialSignature): Future[Transaction] = {
    for {
      unsignedPSBT <- createPartiallySignedRefundTx()
      psbt = unsignedPSBT.addSignature(remoteSig, inputIndex = 0)

      refundTxT = psbt.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      refundTx <- Future.fromTry(refundTxT)
    } yield {
      refundTx
    }
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
}
