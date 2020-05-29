package org.bitcoins.dlc.verify

import org.bitcoins.commons.jsonmodels.dlc.FundingSignatures
import org.bitcoins.core.crypto.{
  TransactionSignatureChecker,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.crypto.Sha256DigestBE
import org.bitcoins.dlc.builder.DLCTxBuilder

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

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
              .addWitnessUTXOToInput(remoteFundingInputs(index).output, idx)
              .addSignatures(sigs, idx)
              .finalizeInput(idx) match {
              case Success(finalized) =>
                finalized.verifyFinalizedInput(idx)
              case Failure(_) =>
                false
            }
          } else false
      }
  }

  def verifyCETSig(outcome: Sha256DigestBE, sig: PartialSignature): Boolean = {
    val cetF = if (isInitiator) {
      builder.buildOfferCET(outcome)
    } else {
      builder.buildAcceptCET(outcome)
    }

    val cet = Await.result(cetF, 5.seconds)

    val sigComponent = WitnessTxSigComponentRaw(transaction = cet,
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
