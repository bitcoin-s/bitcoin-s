package org.bitcoins.dlc.rewrite

import org.bitcoins.core.protocol.dlc.{
  CETCalculator,
  ContractInfo,
  OracleOutcome,
  OracleSignatures
}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.{ECAdaptorSignature, ECPrivateKey, ECPublicKey}
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution.{ExecutedDLCOutcome, RefundDLCOutcome}
import org.bitcoins.dlc.sign.DLCTxSigner

import scala.concurrent.{ExecutionContext, Future}

object DLCExecutor {

  def executeDLC(
      remoteAdaptorSigs: Vector[(OracleOutcome, ECAdaptorSignature)],
      oracleSigs: Vector[OracleSignatures],
      fundingKey: ECPrivateKey,
      remoteFundingPubKey: ECPublicKey,
      contractInfo: ContractInfo, // TODO: compute in overload
      fundingTx: Transaction, // TODO: compute in overload
      ucet: WitnessTransaction // TODO: compute in overload
  )(implicit ec: ExecutionContext): Future[ExecutedDLCOutcome] = {
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
    val sigsUsed =
      sigsUsedOpt.get // Safe because msgOpt is defined if no throw

    val (fundingMultiSig, _) = DLCTxBuilder.buildFundingSPKs(
      Vector(fundingKey.publicKey, remoteFundingPubKey))

    val cetF = DLCTxSigner.completeCET(msg,
                                       fundingKey,
                                       fundingMultiSig,
                                       fundingTx,
                                       ucet,
                                       remoteAdaptorSig,
                                       remoteFundingPubKey,
                                       sigsUsed)

    cetF.map { cet =>
      ExecutedDLCOutcome(fundingTx, cet, msg, sigsUsed)
    }
  }

  def executeRefund(
      fundingTx: Transaction, // TODO: compute in overload
      refundTx: WitnessTransaction // TODO: compute in overload
  ): RefundDLCOutcome = {
    RefundDLCOutcome(fundingTx, refundTx)
  }
}
