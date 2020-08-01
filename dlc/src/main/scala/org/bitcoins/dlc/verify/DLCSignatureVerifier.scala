package org.bitcoins.dlc.verify

import org.bitcoins.commons.jsonmodels.dlc.FundingSignatures
import org.bitcoins.core.crypto.{
  TransactionSignatureChecker,
  TransactionSignatureSerializer,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256DigestBE}
import org.bitcoins.dlc.builder.DLCTxBuilder
import scodec.bits.ByteVector

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

/** Responsible for verifying all DLC signatures */
case class DLCSignatureVerifier(builder: DLCTxBuilder, isInitiator: Boolean) {
  private lazy val fundingTx = Await.result(builder.buildFundingTx, 5.seconds)

  def verifyRemoteFundingSigs(remoteSigs: FundingSignatures): Boolean = {
    val (remoteTweak, remoteFundingInputs) = if (isInitiator) {
      (builder.offerFundingInputs.length, builder.acceptFundingInputs)
    } else {
      (0, builder.offerFundingInputs)
    }

    val psbt = PSBT.fromUnsignedTx(fundingTx)

    remoteSigs.zipWithIndex
      .foldLeft(true) {
        case (ret, ((outPoint, sigs), index)) =>
          if (ret) {
            require(psbt.transaction.inputs(index).previousOutput == outPoint,
                    "Adding signature for incorrect input")

            val idx = index + remoteTweak

            psbt
              .addSignatures(sigs, idx)
              .addWitnessUTXOToInput(remoteFundingInputs(index).output, idx)
              .finalizeInput(idx) match {
              case Success(finalized) =>
                finalized.verifyFinalizedInput(idx)
              case Failure(_) =>
                false
            }
          } else false
      }
  }

  /** Verifies remote's CET signature for a given outcome hash */
  def verifyCETSig(
      outcome: Sha256DigestBE,
      sig: ECAdaptorSignature): Boolean = {
    val remoteFundingPubKey = if (isInitiator) {
      builder.acceptFundingKey
    } else {
      builder.offerFundingKey
    }

    val adaptorPoint = builder.sigPubKeys(outcome)

    val cet = Await.result(builder.buildCET(outcome), 5.seconds)

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

  /** Verifies remote's refund signature */
  def verifyRefundSig(sig: PartialSignature): Boolean = {
    val refundTx = Await.result(builder.buildRefundTx, 5.seconds)

    val sigComponent = WitnessTxSigComponentRaw(transaction = refundTx,
                                                inputIndex = UInt32.zero,
                                                output = fundingTx.outputs.head,
                                                flags = Policy.standardFlags)

    TransactionSignatureChecker
      .checkSignature(
        sigComponent,
        sigComponent.output.scriptPubKey.asm.toVector,
        sig.pubKey,
        sig.signature,
        Policy.standardFlags
      )
      .isValid
  }
}
