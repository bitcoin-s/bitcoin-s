package org.bitcoins.dlc.rewrite

import org.bitcoins.core.crypto.{
  TransactionSignatureChecker,
  TransactionSignatureSerializer,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.{
  DLCFundingInput,
  FundingSignatures,
  OracleOutcome
}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.crypto.{ECAdaptorSignature, ECPublicKey}
import scodec.bits.ByteVector

import scala.util.{Failure, Success}

object DLCValidator extends BitcoinSLogger {

  // TODO: add method for Vector[(OracleOutcome, ECAdaptorSignature)] once implemented
  def validateCETSignature(
      outcome: OracleOutcome,
      sig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      fundingTx: Transaction, // TODO: compute in overload
      cet: WitnessTransaction // TODO: compute in overload
  ): Boolean = {
    val adaptorPoint = outcome.sigPoint

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

  def validateRefundSignature(
      refundSig: PartialSignature,
      fundingTx: Transaction, // TODO: compute in overload
      refundTx: WitnessTransaction // TODO: compute in overload
  ): Boolean = {
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

  // TODO: Don't use PSBT
  def validateFundingSigs(
      fundingTx: Transaction, // TODO: compute in overload
      fundingSigs: FundingSignatures,
      isInitiator: Boolean,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput]): Boolean = {
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
}
