package org.bitcoins.dlc.execution

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.sign.DLCTxSigner

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing SetupDLCs and DLCOutcomes */
case class DLCExecutor(signer: DLCTxSigner)(implicit ec: ExecutionContext) {
  val builder: DLCTxBuilder = signer.builder
  val isInitiator: Boolean = signer.isInitiator

  /** Constructs the initiator's SetupDLC given the non-initiator's
    * CETSignatures which should arrive in a DLC accept message
    */
  def setupDLCOffer(cetSigs: CETSignatures): Future[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    setupDLC(cetSigs, None, None)
  }

  /** Constructs the non-initiator's SetupDLC given the initiator's
    * CETSignatures and FundingSignatures which should arrive in
    * a DLC sign message
    */
  def setupDLCAccept(
      cetSigs: CETSignatures,
      fundingSigs: FundingSignatures,
      cetsOpt: Option[Vector[WitnessTransaction]]): Future[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    setupDLC(cetSigs, Some(fundingSigs), cetsOpt)
  }

  /** Constructs a SetupDLC given the necessary signature information
    * from the counter-party.
    */
  def setupDLC(
      cetSigs: CETSignatures,
      fundingSigsOpt: Option[FundingSignatures],
      cetsOpt: Option[Vector[WitnessTransaction]]): Future[SetupDLC] = {
    if (!isInitiator) {
      require(fundingSigsOpt.isDefined,
              "Accepting party must provide remote funding signatures")
    }

    val CETSignatures(outcomeSigs, refundSig) = cetSigs
    val msgs = outcomeSigs.map(_._1)
    val cetsF = cetsOpt match {
      case Some(cets) => Future.successful(cets)
      case None       => builder.buildCETs(msgs)
    }
    val cetInfosF = cetsF.map { cets =>
      cets.zip(outcomeSigs).map { case (cet, (msg, remoteAdaptorSig)) =>
        msg -> CETInfo(cet, remoteAdaptorSig)
      }
    }

    for {
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.completeFundingTx(fundingSigs)
          case None              => builder.buildFundingTx
        }
      }
      cetInfos <- cetInfosF
      refundTx <- signer.completeRefundTx(refundSig)
    } yield {
      SetupDLC(fundingTx, cetInfos, refundTx)
    }
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sigs: Vector[OracleSignatures]): CurrencyUnit = {
    signer.getPayout(sigs)
  }

  /** Computes closing transactions from a DLCSetup and a set of OracleSignatures.
    * The Vector[OracleSignatures] may contain more OracleSignatures than are needed.
    *
    * TODO: Test over-sharing of OracleSignatures
    */
  def executeDLC(
      dlcSetup: SetupDLC,
      oracleSigs: Vector[OracleSignatures]): Future[ExecutedDLCOutcome] = {

    val threshold = dlcSetup.cets.head._1.oracles.length
    val sigCombinations = CETCalculator.combinations(oracleSigs, threshold)

    var msgOpt: Option[OracleOutcome] = None
    val sigsUsedOpt = sigCombinations.find { sigs =>
      msgOpt = builder.contractInfo.findOutcome(sigs)
      msgOpt.isDefined
    }
    val (msg, remoteAdaptorSig) = msgOpt match {
      case Some(msg) =>
        val cetInfo = dlcSetup.getCETInfo(msg)
        (msg, cetInfo.remoteSignature)
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to any possible outcome! $oracleSigs")
    }
    val sigsUsed = sigsUsedOpt.get // Safe because msgOpt is defined if no throw

    signer.completeCET(msg, remoteAdaptorSig, sigsUsed).map { cet =>
      ExecutedDLCOutcome(dlcSetup.fundingTx, cet, msg, sigsUsed)
    }
  }

  def executeRefundDLC(dlcSetup: SetupDLC): RefundDLCOutcome = {
    val SetupDLC(fundingTx, _, refundTx) = dlcSetup
    RefundDLCOutcome(fundingTx, refundTx)
  }
}
