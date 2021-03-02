package org.bitcoins.dlc.execution

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.{AdaptorSign, ECPublicKey}
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
    val cets = cetsOpt match {
      case Some(cets) => cets
      case None       => builder.buildCETs(msgs)
    }
    val cetInfos =
      cets.zip(outcomeSigs).map { case (cet, (msg, remoteAdaptorSig)) =>
        msg -> CETInfo(cet, remoteAdaptorSig)
      }

    for {
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.completeFundingTx(fundingSigs)
          case None              => Future.successful(builder.buildFundingTx)
        }
      }
    } yield {
      val refundTx = signer.completeRefundTx(refundSig)

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
      oracleSigs: Vector[OracleSignatures]): ExecutedDLCOutcome = {
    val remoteFundingPubKey = if (isInitiator) {
      builder.acceptFundingKey
    } else {
      builder.offerFundingKey
    }

    DLCExecutor.executeDLC(dlcSetup.cets,
                           oracleSigs,
                           signer.fundingKey,
                           remoteFundingPubKey,
                           builder.contractInfo,
                           dlcSetup.fundingTx,
                           builder.fundOutputIndex)
  }

  def executeRefundDLC(dlcSetup: SetupDLC): RefundDLCOutcome = {
    val SetupDLC(fundingTx, _, refundTx) = dlcSetup
    RefundDLCOutcome(fundingTx, refundTx)
  }
}

object DLCExecutor {

  /** Given DLC setup data and oracle signatures, computes the OracleOutcome and a fully signed CET.
    *
    * This function will fail if no threshold-sized subset of the oracle signatures corresponds to
    * a valid set of expected oracle signatures as per the oracle announcements in the ContractInfo.
    */
  def executeDLC(
      remoteCETInfos: Vector[(OracleOutcome, CETInfo)],
      oracleSigs: Vector[OracleSignatures],
      fundingKey: AdaptorSign,
      remoteFundingPubKey: ECPublicKey,
      contractInfo: ContractInfo,
      fundingTx: Transaction,
      fundOutputIndex: Int
  ): ExecutedDLCOutcome = {
    val threshold = contractInfo.oracleInfo.threshold
    val sigCombinations = CETCalculator.combinations(oracleSigs, threshold)

    var msgOpt: Option[OracleOutcome] = None
    val sigsUsedOpt = sigCombinations.find { sigs =>
      msgOpt = contractInfo.findOutcome(sigs)
      msgOpt.isDefined
    }

    val msgAndCETInfoOpt = msgOpt.flatMap { msg =>
      remoteCETInfos.find(_._1 == msg)
    }

    val (msg, ucet, remoteAdaptorSig) = msgAndCETInfoOpt match {
      case Some((msg, CETInfo(ucet, remoteSig))) => (msg, ucet, remoteSig)
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to any possible outcome! $oracleSigs")
    }
    val sigsUsed =
      sigsUsedOpt.get // Safe because msgOpt is defined if no throw

    val (fundingMultiSig, _) = DLCTxBuilder.buildFundingSPKs(
      Vector(fundingKey.publicKey, remoteFundingPubKey))

    val signingInfo =
      DLCTxSigner.buildCETSigningInfo(fundOutputIndex,
                                      fundingTx,
                                      fundingMultiSig,
                                      fundingKey)

    val cet = DLCTxSigner.completeCET(msg,
                                      signingInfo,
                                      fundingMultiSig,
                                      fundingTx,
                                      ucet,
                                      remoteAdaptorSig,
                                      remoteFundingPubKey,
                                      sigsUsed)

    ExecutedDLCOutcome(fundingTx, cet, msg, sigsUsed)
  }
}
