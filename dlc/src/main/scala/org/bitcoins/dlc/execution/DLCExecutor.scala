package org.bitcoins.dlc.execution

import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, FundingSignatures}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  ScriptPubKey,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  P2WPKHV0InputInfo,
  P2WSHV0InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.sign.DLCTxSigner

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing SetupDLCs and DLCOutcomes */
case class DLCExecutor(signer: DLCTxSigner) extends BitcoinSLogger {
  implicit private val ec: ExecutionContext = signer.ec

  val builder: DLCTxBuilder = signer.builder

  val feeRate: SatoshisPerVirtualByte = builder.feeRate

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
    val localCetDataFs = outcomeSigs.map {
      case (msg, sig) =>
        for {
          tx <- signer.signCET(msg, sig)
          witness <- builder.getCETWitness(msg, isInitiator)
        } yield {
          msg -> (tx, witness)
        }
    }
    val remoteCETDataFs = messages.map { msg =>
      for {
        utx <- builder.buildCET(msg, !isInitiator)
        witness <- builder.getCETWitness(msg, !isInitiator)
      } yield {
        msg -> (utx, witness)
      }
    }

    for {
      localCetData <- Future.sequence(localCetDataFs).map(_.toMap)
      remoteCetData <- Future.sequence(remoteCETDataFs).map(_.toMap)
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.signFundingTx(fundingSigs)
          case None              => builder.buildFundingTx
        }
      }
      refundTx <- signer.signRefundTx(refundSig)
    } yield {
      val cetInfos = CETInfo.mapFromMaps(localCetData, remoteCetData)

      SetupDLC(
        fundingTx,
        cetInfos,
        refundTx
      )
    }
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sig: SchnorrDigitalSignature): CurrencyUnit = {
    signer.getPayout(sig)
  }

  /** Constructs and executes on the unilateral spending branch of a DLC
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#closing-transaction-unilateral]]
    *
    * @return Each transaction published and its spending info
    */
  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSig: SchnorrDigitalSignature): Future[UnilateralDLCOutcome] = {
    val SetupDLC(fundingTx, cetInfos, _) = dlcSetup

    val msgOpt =
      messages.find(msg => builder.oraclePubKey.verify(msg.bytes, oracleSig))
    val (msg, cet, cetScriptWitness) = msgOpt match {
      case Some(msg) =>
        val cetInfo = cetInfos(msg)
        (msg, cetInfo.tx, cetInfo.witness)
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to any possible outcome! $oracleSig")
    }

    val output = cet.outputs.head

    output.scriptPubKey match {
      case _: P2WSHWitnessSPKV0 =>
        val localSpendingTxF =
          constructUnilateralCETSpend(cet, cetScriptWitness, msg, oracleSig)

        localSpendingTxF.map {
          case Some((localSpendingTx, cetSpendingInfo)) =>
            UnilateralDLCOutcomeWithClosing(
              fundingTx = fundingTx,
              cet = cet,
              closingTx = localSpendingTx,
              cetSpendingInfo = cetSpendingInfo
            )
          case None =>
            UnilateralDLCOutcomeWithDustClosing(fundingTx = fundingTx,
                                                cet = cet)
        }
      case _: ScriptPubKey =>
        Future.successful(
          UnilateralDLCOutcomeWithDustClosing(fundingTx = fundingTx, cet = cet)
        )
    }
  }

  /** Constructs the closing transaction on the to_remote output of a counter-party's unilateral CET broadcast
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#closing-transaction-unilateral]]
    */
  def executeRemoteUnilateralDLC(
      dlcSetup: SetupDLC,
      publishedCET: Transaction,
      sweepSPK: WitnessScriptPubKey): Future[UnilateralDLCOutcome] = {
    val output = publishedCET.outputs.last

    output.scriptPubKey match {
      case _: P2WSHWitnessSPKV0 =>
        Future.successful(
          UnilateralDLCOutcomeWithDustClosing(fundingTx = dlcSetup.fundingTx,
                                              cet = publishedCET)
        )
      case _: ScriptPubKey =>
        val msgOpt =
          dlcSetup.cets.find(_._2.remoteTxid == publishedCET.txIdBE).map(_._1)

        msgOpt match {
          case Some(_) =>
            val spendingInfo = ScriptSignatureParams(
              inputInfo = P2WPKHV0InputInfo(
                outPoint = TransactionOutPoint(publishedCET.txIdBE, UInt32.one),
                amount = output.value,
                pubKey = signer.finalPrivKey.publicKey),
              signer = signer.finalPrivKey,
              hashType = HashType.sigHashAll
            )

            val txF =
              constructClosingTx(spendingInfo = spendingInfo,
                                 sweepSPK = sweepSPK)

            txF.map {
              case Some(tx) =>
                UnilateralDLCOutcomeWithClosing(
                  fundingTx = dlcSetup.fundingTx,
                  cet = publishedCET,
                  closingTx = tx,
                  cetSpendingInfo = spendingInfo
                )
              case None =>
                UnilateralDLCOutcomeWithDustClosing(fundingTx =
                                                      dlcSetup.fundingTx,
                                                    cet = publishedCET)
            }
          case None =>
            throw new IllegalArgumentException(
              s"Published CET $publishedCET does not correspond to any known outcome")
        }
    }
  }

  /** Constructs and executes on the justice spending branch of a DLC
    * where a published CET has timed out.
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#closing-transaction-penalty]]
    *
    * @return Each transaction published and its spending info
    */
  def executeJusticeDLC(
      dlcSetup: SetupDLC,
      timedOutCET: Transaction): Future[UnilateralDLCOutcome] = {
    val justiceOutput = timedOutCET.outputs.head

    val cetScriptWitness =
      dlcSetup.cets.find(_._2.remoteTxid == timedOutCET.txIdBE) match {
        case Some((_, cetInfo)) => cetInfo.remoteWitness
        case None =>
          throw new IllegalArgumentException(
            s"Timed out CET $timedOutCET does not correspond to any known outcome")
      }

    val justiceSpendingInfo = ScriptSignatureParams(
      inputInfo = P2WSHV0InputInfo(
        outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.zero),
        amount = justiceOutput.value,
        scriptWitness = cetScriptWitness,
        conditionalPath = ConditionalPath.nonNestedFalse
      ),
      signer = signer.cetToLocalPrivKey,
      hashType = HashType.sigHashAll
    )

    justiceOutput.scriptPubKey match {
      case _: P2WSHWitnessSPKV0 =>
        val justiceSpendingTxF = constructClosingTx(justiceSpendingInfo)

        justiceSpendingTxF.map {
          case Some(justiceSpendingTx) =>
            UnilateralDLCOutcomeWithClosing(
              fundingTx = dlcSetup.fundingTx,
              cet = timedOutCET,
              closingTx = justiceSpendingTx,
              cetSpendingInfo = justiceSpendingInfo
            )
          case None =>
            UnilateralDLCOutcomeWithDustClosing(fundingTx = dlcSetup.fundingTx,
                                                cet = timedOutCET)
        }
      case _: ScriptPubKey =>
        Future.successful(
          UnilateralDLCOutcomeWithDustClosing(fundingTx = dlcSetup.fundingTx,
                                              cet = timedOutCET)
        )
    }
  }

  /** Constructs and executes on the refund spending branch of a DLC
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#refund-transaction]]
    *
    * @return Each transaction published and its spending info
    */
  def executeRefundDLC(dlcSetup: SetupDLC): Future[RefundDLCOutcome] = {
    val SetupDLC(fundingTx, _, refundTx) =
      dlcSetup

    val (localOutput, vout) = if (isInitiator) {
      (refundTx.outputs.head, UInt32.zero)
    } else {
      (refundTx.outputs.last, UInt32.one)
    }

    val localRefundSpendingInfo = ScriptSignatureParams(
      inputInfo = P2WPKHV0InputInfo(
        outPoint = TransactionOutPoint(refundTx.txIdBE, vout),
        amount = localOutput.value,
        pubKey = signer.finalPrivKey.publicKey
      ),
      signer = signer.finalPrivKey,
      hashType = HashType.sigHashAll
    )

    val localSpendingTxF = constructClosingTx(localRefundSpendingInfo)

    localSpendingTxF.map {
      case Some(localSpendingTx) =>
        RefundDLCOutcomeWithClosing(
          fundingTx = fundingTx,
          refundTx = refundTx,
          closingTx = localSpendingTx,
          refundSpendingInfo = localRefundSpendingInfo
        )
      case None =>
        RefundDLCOutcomeWithDustClosing(fundingTx = fundingTx,
                                        refundTx = refundTx)
    }
  }

  private def constructClosingTx(
      spendingInfo: ScriptSignatureParams[InputInfo],
      sweepSPK: WitnessScriptPubKey = P2WPKHWitnessSPKV0(
        signer.finalPrivKey.publicKey)): Future[Option[Transaction]] = {

    val lockTime = TxUtil.calcLockTime(Vector(spendingInfo))
    val inputs = InputUtil.calcSequenceForInputs(Vector(spendingInfo))

    val builder = RawTxBuilder().setLockTime(lockTime.get) += TransactionOutput(
      spendingInfo.output.value,
      sweepSPK) ++= inputs

    val finalizer = SubtractFeeFromOutputsFinalizer(
      Vector(spendingInfo.inputInfo),
      feeRate,
      Vector(sweepSPK)
    ).andThen(
      SanityCheckFinalizer(
        Vector(spendingInfo.inputInfo),
        Vector(sweepSPK),
        feeRate
      )
    )

    val spendingTxOptF = finalizer
      .buildTx(builder.result())
      .flatMap(utx => RawTxSigner.sign(utx, Vector(spendingInfo), feeRate))
      .map(Some(_))

    spendingTxOptF.foreach(txOpt =>
      logger.info(s"Closing Tx: ${txOpt.map(_.hex)}"))

    spendingTxOptF
  }

  private def constructUnilateralCETSpend(
      cet: Transaction,
      cetScriptWitness: P2WSHWitnessV0,
      msg: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    Option[(Transaction, ScriptSignatureParams[P2WSHV0InputInfo])]] = {
    val payoutValue = if (isInitiator) {
      builder.offerOutcomes(msg)
    } else {
      builder.acceptOutcomes(msg)
    }

    val inputInfo = P2WSHV0InputInfo(
      outPoint = TransactionOutPoint(cet.txIdBE, UInt32.zero),
      amount = cet.outputs.head.value,
      scriptWitness = cetScriptWitness,
      conditionalPath = ConditionalPath.nonNestedTrue
    )

    val privKey = DLCTxSigner.tweakedPrivKey(signer.fundingPrivKey,
                                             signer.cetToLocalPrivKey,
                                             oracleSig)

    val spendingInfo = ScriptSignatureParams(
      inputInfo = inputInfo,
      signer = privKey,
      hashType = HashType.sigHashAll
    )

    if (payoutValue < Policy.dustThreshold) {
      Future.successful(None)
    } else {
      val inputs = InputUtil.calcSequenceForInputs(Vector(spendingInfo))
      val lockTime = TxUtil.calcLockTime(Vector(spendingInfo)).get
      val sweepSPK = P2WPKHWitnessSPKV0(signer.finalPrivKey.publicKey)
      val builder = RawTxBuilder().setLockTime(
        lockTime) ++= inputs += TransactionOutput(payoutValue, sweepSPK)

      val finalizer =
        SanityCheckFinalizer(Vector(inputInfo), Vector(sweepSPK), feeRate)
          .andThen(AddWitnessDataFinalizer(Vector(inputInfo)))

      val utxF = finalizer.buildTx(builder.result())

      val signedTxF = utxF.flatMap { utx =>
        RawTxSigner.sign(utx, Vector(spendingInfo), feeRate)
      }

      signedTxF.foreach(tx => logger.info(s"Closing Tx: ${tx.hex}"))

      signedTxF.map(signedTx => Some((signedTx, spendingInfo)))
    }
  }
}
