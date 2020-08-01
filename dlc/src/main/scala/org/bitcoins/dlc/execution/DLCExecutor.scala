package org.bitcoins.dlc.execution

import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, FundingSignatures}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.sign.DLCTxSigner

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing SetupDLCs and DLCOutcomes */
case class DLCExecutor(signer: DLCTxSigner)(implicit ec: ExecutionContext) {
  val builder: DLCTxBuilder = signer.builder
  val isInitiator: Boolean = signer.isInitiator
  val messages: Vector[Sha256DigestBE] = builder.offerOutcomes.keys.toVector

  /** Constructs the initiator's SetupDLC given the non-initiator's
    * CETSignatures which should arrive in a DLC accept message
    */
  def setupDLCOffer(cetSigs: CETSignatures): Future[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    setupDLC(cetSigs, None)
  }

  /** Constructs the non-initiator's SetupDLC given the initiator's
    * CETSignatures and FundingSignatures which should arrive in
    * a DLC sign message
    */
  def setupDLCAccept(
      cetSigs: CETSignatures,
      fundingSigs: FundingSignatures): Future[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    setupDLC(cetSigs, Some(fundingSigs))
  }

  /** Constructs a SetupDLC given the necessary signature information
    * from the counter-party.
    */
  def setupDLC(
      cetSigs: CETSignatures,
      fundingSigsOpt: Option[FundingSignatures]): Future[SetupDLC] = {
    if (!isInitiator) {
      require(fundingSigsOpt.isDefined,
              "Accepting party must provide remote funding signatures")
    }

    val CETSignatures(outcomeSigs, refundSig) = cetSigs
    val cetInfoFs = outcomeSigs.map {
      case (msg, remoteAdaptorSig) =>
        builder.buildCET(msg).map { cet =>
          msg -> CETInfo(cet, remoteAdaptorSig)
        }
    }

    for {
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.signFundingTx(fundingSigs)
          case None              => builder.buildFundingTx
        }
      }
      cetInfos <- Future.sequence(cetInfoFs)
      refundTx <- signer.signRefundTx(refundSig)
    } yield {
      SetupDLC(fundingTx, cetInfos.toMap, refundTx)
    }
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sig: SchnorrDigitalSignature): CurrencyUnit = {
    signer.getPayout(sig)
  }

  def executeDLC(
      dlcSetup: SetupDLC,
      oracleSig: SchnorrDigitalSignature): Future[ExecutedDLCOutcome] = {
    val SetupDLC(fundingTx, cetInfos, _) = dlcSetup

    val msgOpt =
      messages.find(msg => builder.oraclePubKey.verify(msg.bytes, oracleSig))
    val (msg, remoteAdaptorSig) = msgOpt match {
      case Some(msg) =>
        val cetInfo = cetInfos(msg)
        (msg, cetInfo.remoteSignature)
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to any possible outcome! $oracleSig")
    }

    signer.signCET(msg, remoteAdaptorSig, oracleSig).map { cet =>
      ExecutedDLCOutcome(fundingTx, cet)
    }
  }

  def executeRefundDLC(dlcSetup: SetupDLC): RefundDLCOutcome = {
    val SetupDLC(fundingTx, _, refundTx) = dlcSetup
    RefundDLCOutcome(fundingTx, refundTx)
  }
}
