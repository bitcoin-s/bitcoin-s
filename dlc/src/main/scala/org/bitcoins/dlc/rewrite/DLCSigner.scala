package org.bitcoins.dlc.rewrite

import org.bitcoins.core.crypto.TransactionSignatureSerializer
import org.bitcoins.core.protocol.dlc.{
  DLCFundingInput,
  FundingSignatures,
  OracleOutcome,
  OracleSignatures
}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  NonWitnessTransaction,
  Transaction,
  TransactionOutPoint,
  WitnessTransaction
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo.{
  ECSignatureParams,
  InputInfo,
  P2WSHV0InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.{ECAdaptorSignature, ECPrivateKey, ECPublicKey}

import scala.concurrent.{ExecutionContext, Future}

object DLCSigner {

  def adaptorSignCET(
      outcome: OracleOutcome,
      cet: WitnessTransaction, // TODO: compute in overload
      cetSigningInfo: ECSignatureParams[
        P2WSHV0InputInfo
      ], // TODO: compute in overload
      fundingKey: ECPrivateKey): ECAdaptorSignature = {
    val adaptorPoint = outcome.sigPoint
    val hashToSign =
      TransactionSignatureSerializer.hashForSignature(cet,
                                                      cetSigningInfo,
                                                      HashType.sigHashAll)

    fundingKey.adaptorSign(adaptorPoint, hashToSign.bytes)
  }

  def adaptorSignCETs(
      outcomesAndCETs: Vector[
        (OracleOutcome, WitnessTransaction)
      ], // TODO: compute in overload
      cetSigningInfo: ECSignatureParams[
        P2WSHV0InputInfo
      ], // TODO: compute in overload
      fundingKey: ECPrivateKey): Vector[(OracleOutcome, ECAdaptorSignature)] = {
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

  // TODO: Without PSBTs
  def completeCET(
      outcome: OracleOutcome,
      fundingKey: ECPrivateKey,
      fundingTx: Transaction, // TODO: compute in overload
      ucet: WitnessTransaction, // TODO: compute in overload
      remoteAdaptorSig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      oracleSigs: Vector[OracleSignatures])(implicit
      ec: ExecutionContext): Future[WitnessTransaction] = {
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
          .fromUnsignedTx(ucet)
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
}
