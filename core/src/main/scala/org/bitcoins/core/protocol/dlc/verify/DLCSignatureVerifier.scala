package org.bitcoins.core.protocol.dlc.verify

import org.bitcoins.core.crypto.{
  TaprootSerializationOptions,
  TransactionSignatureChecker,
  TransactionSignatureSerializer,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models.{
  DLCFundingInput,
  FundingSignatures
}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.{FutureUtil, Indexed}
import org.bitcoins.crypto.{
  ECAdaptorSignature,
  ECDigitalSignature,
  ECPublicKey,
  HashType
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Responsible for verifying all DLC signatures */
case class DLCSignatureVerifier(builder: DLCTxBuilder, isInitiator: Boolean) {
  private def fundingTx: Transaction = builder.buildFundingTx

  def verifyRemoteFundingSigs(remoteSigs: FundingSignatures): Boolean = {
    DLCSignatureVerifier.validateRemoteFundingSigs(fundingTx,
                                                   remoteSigs,
                                                   isInitiator,
                                                   builder.offerFundingInputs,
                                                   builder.acceptFundingInputs)
  }

  /** Verifies remote's CET signature for a given outcome hash */
  def verifyCETSig(
      adaptorPoint: Indexed[ECPublicKey],
      sig: ECAdaptorSignature): Boolean = {
    val remoteFundingPubKey = if (isInitiator) {
      builder.acceptFundingKey
    } else {
      builder.offerFundingKey
    }
    val cet = builder.buildCET(adaptorPoint)

    DLCSignatureVerifier.validateCETSignature(adaptorPoint.element,
                                              sig,
                                              remoteFundingPubKey,
                                              fundingTx,
                                              builder.fundOutputIndex,
                                              cet)
  }

  def verifyCETSigs(sigs: Vector[(Indexed[ECPublicKey], ECAdaptorSignature)])(
      implicit ec: ExecutionContext): Future[Boolean] = {
    val correctNumberOfSigs =
      sigs.size >= builder.contractInfo.allOutcomes.length

    val verifyFn: Vector[(Indexed[ECPublicKey], ECAdaptorSignature)] => Future[
      Boolean] = { outcomeSigs =>
      FutureUtil.makeAsync { () =>
        outcomeSigs.forall { case (outcome, sig) =>
          verifyCETSig(outcome, sig)
        }
      }
    }

    if (correctNumberOfSigs) {
      FutureUtil
        .batchAndParallelExecute(sigs, verifyFn)
        .map(_.forall(res => res))
    } else Future.successful(false)
  }

  /** Verifies remote's refund signature */
  def verifyRefundSig(sig: PartialSignature[ECDigitalSignature]): Boolean = {
    val refundTx = builder.buildRefundTx

    DLCSignatureVerifier.validateRefundSignature(sig,
                                                 fundingTx,
                                                 builder.fundOutputIndex,
                                                 refundTx)
  }
}

object DLCSignatureVerifier {

  def validateCETSignature(
      adaptorPoint: ECPublicKey,
      sig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      fundingTx: Transaction,
      fundOutputIndex: Int,
      cet: WitnessTransaction
  ): Boolean = {
    val sigComponent = WitnessTxSigComponentRaw(
      transaction = cet,
      inputIndex = UInt32.zero,
      output = fundingTx.outputs(fundOutputIndex),
      flags = Policy.standardFlags)

    val hashType = HashType(
      ByteVector(0.toByte, 0.toByte, 0.toByte, HashType.sigHashAll.byte))
    val hash =
      TransactionSignatureSerializer.hashForSignature(
        txSigComponent = sigComponent,
        hashType = hashType,
        taprootOptions = TaprootSerializationOptions.empty)

    remoteFundingPubKey.adaptorVerify(hash.bytes, adaptorPoint, sig)
  }

  def validateRefundSignature(
      refundSig: PartialSignature[ECDigitalSignature],
      fundingTx: Transaction,
      fundOutputIndex: Int,
      refundTx: WitnessTransaction
  ): Boolean = {
    val sigComponent = WitnessTxSigComponentRaw(
      transaction = refundTx,
      inputIndex = UInt32.zero,
      output = fundingTx.outputs(fundOutputIndex),
      flags = Policy.standardFlags)

    TransactionSignatureChecker
      .checkSignature(
        sigComponent,
        sigComponent.fundingOutput.scriptPubKey.asm.toVector,
        refundSig.pubKey,
        refundSig.signature,
        Policy.standardFlags
      )
      .isValid
  }

  // TODO: Without PSBTs
  def validateRemoteFundingSigs(
      fundingTx: Transaction,
      fundingSigs: FundingSignatures,
      localIsInitiator: Boolean,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput]): Boolean = {
    val fundingInputs = offerFundingInputs ++ acceptFundingInputs

    val serialIdMap =
      fundingInputs
        .map(input => input.outPoint -> input.inputSerialId)
        .toMap

    val serialIds = fundingInputs.map(_.inputSerialId).sorted

    val remoteFundingInputs =
      if (localIsInitiator) acceptFundingInputs
      else offerFundingInputs

    val psbt = PSBT.fromUnsignedTxWithP2SHScript(fundingTx)

    fundingSigs
      .foldLeft(true) { case (ret, (outPoint, witness)) =>
        val serialId = serialIdMap(outPoint)
        val idx = serialIds.indexOf(serialId)
        if (ret) {
          if (psbt.transaction.inputs(idx).previousOutput != outPoint) {
            // TODO: Replace with error log
            println("Adding signature for incorrect input")

            false
          } else {
            Try {
              val fundingInput =
                remoteFundingInputs.find(_.outPoint == outPoint) match {
                  case Some(input) => input
                  case None =>
                    throw new RuntimeException(
                      s"Could not find fundingInput for outpoint $outPoint")
                }

              psbt
                .addUTXOToInput(fundingInput.prevTx, idx)
                .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                                  witness,
                                                  idx)
                .finalizeInput(idx)
            }.flatten match {
              case Success(finalized) =>
                finalized.verifyFinalizedInput(idx)
              case Failure(_) =>
                false
            }
          }
        } else false
      }
  }
}
