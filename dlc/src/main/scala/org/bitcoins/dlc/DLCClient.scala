package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCMutualCloseSig
}
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.sign.DLCTxSigner
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

/** This case class allows for the construction and execution of
  * Discreet Log Contracts between two parties.
  *
  * @param offer The DLCOffer associated with this DLC
  * @param accept The DLCAccept (without sigs) associated with this DLC
  * @param isInitiator True if this client sends the offer message
  * @param extPrivKey This client's extended private key (at the account level) for this event
  * @param nextAddressIndex The next unused address index for the provided extPrivKey
  * @param fundingUtxos This client's funding BitcoinUTXOSpendingInfo collection
  */
case class DLCClient(
    offer: DLCMessage.DLCOffer,
    accept: DLCMessage.DLCAcceptWithoutSigs,
    isInitiator: Boolean,
    extPrivKey: ExtPrivateKey,
    nextAddressIndex: Int,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(
    implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  private val dlcTxBuilder = DLCTxBuilder(offer, accept)

  private val dlcTxSigner = DLCTxSigner(dlcTxBuilder,
                                        isInitiator,
                                        extPrivKey,
                                        nextAddressIndex,
                                        fundingUtxos)

  private val remotePubKeys = if (isInitiator) {
    accept.pubKeys
  } else {
    offer.pubKeys
  }

  private val outcomes = if (isInitiator) {
    offer.contractInfo
  } else {
    dlcTxBuilder.acceptOutcomes
  }

  val messages: Vector[Sha256DigestBE] = outcomes.keys.toVector

  private val feeRate = offer.feeRate

  private val remoteFundingInputs = if (isInitiator) {
    accept.fundingInputs
  } else {
    offer.fundingInputs
  }

  private val oraclePubKey = offer.oracleInfo.pubKey

  val timeouts: DLCTimeouts = offer.timeouts

  val fundingPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/$nextAddressIndex"))
      .key

  val cetToLocalPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 1}"))
      .key

  val finalPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 2}"))
      .key

  val fundingRemotePubKey: ECPublicKey = remotePubKeys.fundingKey

  val cetToLocalRemotePubKey: ECPublicKey = remotePubKeys.toLocalCETKey

  val finalRemoteScriptPubKey: ScriptPubKey =
    remotePubKeys.finalAddress.scriptPubKey

  val fundingPubKey: ECPublicKey = fundingPrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    val fundingKeys = if (isInitiator) {
      Vector(fundingPubKey, fundingRemotePubKey)
    } else {
      Vector(fundingRemotePubKey, fundingPubKey)
    }

    MultiSignatureScriptPubKey(2, fundingKeys)
  }

  lazy val createUnsignedFundingTransaction: Transaction =
    Await.result(dlcTxBuilder.buildFundingTx, 5.seconds)

  lazy val fundingTxId: DoubleSha256Digest =
    createUnsignedFundingTransaction.txId

  lazy val fundingOutput: TransactionOutput =
    createUnsignedFundingTransaction.outputs.head

  lazy val fundingInputInfo: P2WSHV0InputInfo = P2WSHV0InputInfo(
    TransactionOutPoint(fundingTxId, UInt32.zero),
    fundingOutput.value,
    P2WSHWitnessV0(fundingSPK),
    ConditionalPath.NoCondition)

  def getPayout(oracleSig: SchnorrDigitalSignature): CurrencyUnit = {
    val (offerPayout, acceptPayout) = dlcTxBuilder.getPayouts(oracleSig)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  def createFundingTransactionSigs(): Future[FundingSignatures] = {
    dlcTxSigner.createFundingTxSigs()
  }

  def verifyRemoteFundingSigs(remoteSigs: FundingSignatures): Boolean = {
    val remoteTweak = if (isInitiator) {
      fundingUtxos.length
    } else {
      0
    }

    val psbt = PSBT.fromUnsignedTx(createUnsignedFundingTransaction)

    remoteSigs.zipWithIndex
      .foldLeft(true) {
        case (ret, ((outPoint, sigs), index)) =>
          if (ret) {
            require(psbt.transaction.inputs(index).previousOutput == outPoint,
                    "Adding signature for incorrect input")

            val idx = index + remoteTweak

            // TODO: add funding witness and redeem scripts
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

  /** Creates a ready-to-sign PSBT for the Mutual Close tx
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#mutual-closing-transaction]]
    *
    * @param sig The oracle's signature for this contract
    */
  def createUnsignedMutualCloseTx(sig: SchnorrDigitalSignature): Transaction = {
    val utxF = dlcTxBuilder.buildMutualCloseTx(sig)

    Await.result(utxF, 5.seconds)
  }

  def createMutualCloseTx(
      sig: SchnorrDigitalSignature,
      fundingSig: PartialSignature): Future[Transaction] = {
    dlcTxSigner.signMutualCloseTx(sig, fundingSig)
  }

  def verifyCETSig(outcome: Sha256DigestBE, sig: PartialSignature): Boolean = {
    val cetF = if (isInitiator) {
      dlcTxBuilder.buildOfferCET(outcome)
    } else {
      dlcTxBuilder.buildAcceptCET(outcome)
    }

    val cet = Await.result(cetF, 5.seconds)
    val fundingTx = createUnsignedFundingTransaction

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
    val refundTx = Await.result(dlcTxBuilder.buildRefundTx, 5.seconds)
    val fundingTx = createUnsignedFundingTransaction

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

  def createCETSigs: Future[CETSignatures] = {
    dlcTxSigner.createCETSigs()
  }

  /** Executes DLC setup for the party responding to the initiator.
    *
    * This party is the first to send signatures but does not send funding
    * tx signatures.
    *
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#accept]]
    *
    * @param sendSigs The function by which this party sends their CET signatures to the initiator
    * @param getSigs The future which becomes populated by the initiator's CET and funding signatures
    *                (note that this will complete only after the receipt of this client's signatures)
    */
  def setupDLCAccept(
      sendSigs: CETSignatures => Future[Unit],
      getSigs: Future[(CETSignatures, FundingSignatures)]): Future[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    for {
      remoteCetSigs <- dlcTxSigner.createCETSigs()
      _ <- sendSigs(remoteCetSigs)
      (cetSigs, fundingSigs) <- getSigs

      localCetDataFs = cetSigs.outcomeSigs.map {
        case (msg, sig) =>
          for {
            tx <- dlcTxSigner.signCET(msg, sig)
            witness <- {
              if (isInitiator) {
                dlcTxBuilder.getOfferCETWitness(msg)
              } else {
                dlcTxBuilder.getAcceptCETWitness(msg)
              }
            }
          } yield {
            msg -> (tx, witness)
          }
      }
      localCetData <- Future.sequence(localCetDataFs).map(_.toMap)

      remoteCETDataFs = messages.map { msg =>
        if (isInitiator) {
          for {
            utx <- dlcTxBuilder.buildAcceptCET(msg)
            witness <- dlcTxBuilder.getAcceptCETWitness(msg)
          } yield msg -> (utx, witness)
        } else {
          for {
            utx <- dlcTxBuilder.buildOfferCET(msg)
            witness <- dlcTxBuilder.getOfferCETWitness(msg)
          } yield msg -> (utx, witness)
        }
      }
      remoteCETData <- Future.sequence(remoteCETDataFs).map(_.toMap)

      fundingTx <- dlcTxSigner.signFundingTx(fundingSigs)
      refundTx <- dlcTxSigner.signRefundTx(cetSigs.refundSig)
    } yield {
      val cetInfos = messages.map { msg =>
        val (localCet, localWitness) = localCetData(msg)
        val (remoteCet, remoteWitness) = remoteCETData(msg)
        msg -> CETInfo(localCet, localWitness, remoteCet.txIdBE, remoteWitness)
      }.toMap

      SetupDLC(
        fundingTx,
        cetInfos,
        refundTx
      )
    }
  }

  /** Executes DLC setup for the initiating party.
    *
    * This party is the first to send signatures but does not send funding
    * tx signatures.
    *
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#sign]]
    *
    * @param getSigs The future which becomes populated with the other party's CET signatures
    * @param sendSigs The function by which this party sends their CET and funding signatures to the other party
    */
  def setupDLCOffer(
      getSigs: Future[CETSignatures],
      sendSigs: (CETSignatures, FundingSignatures) => Future[Unit],
      getFundingTx: Future[Transaction]): Future[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    getSigs.flatMap {
      case CETSignatures(outcomeSigs, refundSig) =>
        // Construct all CETs
        val localCetDataFs = outcomeSigs.map {
          case (msg, sig) =>
            for {
              tx <- dlcTxSigner.signCET(msg, sig)
              witness <- {
                if (isInitiator) {
                  dlcTxBuilder.getOfferCETWitness(msg)
                } else {
                  dlcTxBuilder.getAcceptCETWitness(msg)
                }
              }
            } yield {
              msg -> (tx, witness)
            }
        }
        val remoteCETDataFs = messages.map { msg =>
          if (isInitiator) {
            for {
              utx <- dlcTxBuilder.buildAcceptCET(msg)
              witness <- dlcTxBuilder.getAcceptCETWitness(msg)
            } yield msg -> (utx, witness)
          } else {
            for {
              utx <- dlcTxBuilder.buildOfferCET(msg)
              witness <- dlcTxBuilder.getOfferCETWitness(msg)
            } yield msg -> (utx, witness)
          }
        }

        for {
          localCetData <- Future.sequence(localCetDataFs).map(_.toMap)
          remoteCetData <- Future.sequence(remoteCETDataFs).map(_.toMap)
          refundTx <- dlcTxSigner.signRefundTx(refundSig)
          cetSigs <- dlcTxSigner.createCETSigs()
          localFundingSigs <- dlcTxSigner.createFundingTxSigs()
          _ <- sendSigs(cetSigs, localFundingSigs)
          fundingTx <- getFundingTx
        } yield {
          val cetInfos = messages.map { msg =>
            val (localCet, localWitness) = localCetData(msg)
            val (remoteCet, remoteWitness) = remoteCetData(msg)
            msg -> CETInfo(localCet,
                           localWitness,
                           remoteCet.txIdBE,
                           remoteWitness)
          }.toMap

          SetupDLC(
            fundingTx,
            cetInfos,
            refundTx
          )
        }
    }
  }

  def constructClosingTx(
      spendingInfo: ScriptSignatureParams[InputInfo],
      msg: Sha256DigestBE,
      spendsToLocal: Boolean,
      sweepSPK: WitnessScriptPubKey = P2WPKHWitnessSPKV0(finalPrivKey.publicKey)): Future[
    Option[Transaction]] = {
    // If spendsToLocal, use payout as value, otherwise subtract fee
    val spendingTxOptF = if (spendsToLocal) {
      val payoutValue = outcomes(msg)

      if (payoutValue < Policy.dustThreshold) {
        Future.successful(None)
      } else {
        val inputs = InputUtil.calcSequenceForInputs(Vector(spendingInfo))
        val lockTime = TxUtil.calcLockTime(Vector(spendingInfo)).get
        val builder = RawTxBuilder().setLockTime(lockTime) ++= inputs += TransactionOutput(
          payoutValue,
          sweepSPK)

        val finalizer = SanityCheckFinalizer(Vector(spendingInfo.inputInfo),
                                             Vector(sweepSPK),
                                             feeRate)
          .andThen(AddWitnessDataFinalizer(Vector(spendingInfo.inputInfo)))

        val utxF = finalizer.buildTx(builder.result())

        val signedTxF = utxF.flatMap { utx =>
          RawTxSigner.sign(utx, Vector(spendingInfo), feeRate)
        }
        signedTxF.map(Some(_))
      }
    } else {
      val lockTime = TxUtil.calcLockTime(Vector(spendingInfo))
      val inputs = InputUtil.calcSequenceForInputs(Vector(spendingInfo))

      val builder = RawTxBuilder().setLockTime(lockTime.get) += TransactionOutput(
        spendingInfo.output.value,
        sweepSPK) ++= inputs

      SubtractFeeFromOutputsFinalizer(Vector(spendingInfo.inputInfo),
                                      feeRate,
                                      Vector(sweepSPK))
        .andThen(
          SanityCheckFinalizer(Vector(spendingInfo.inputInfo),
                               Vector(sweepSPK),
                               feeRate))
        .buildTx(builder.result())
        .flatMap(utx => RawTxSigner.sign(utx, Vector(spendingInfo), feeRate))
        .map(Some(_))
    }

    spendingTxOptF.foreach(txOpt =>
      logger.info(s"Closing Tx: ${txOpt.map(_.hex)}"))

    spendingTxOptF
  }

  def createMutualCloseSig(
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {
    dlcTxSigner.createMutualCloseTxSig(oracleSig)
  }

  /** Initiates a Mutual Close by offering signatures to the counter-party
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#mutual-closing-transaction]]
    *
    * @param sig The oracle's signature on this contract's event
    * @param sendSigs The function by which signatures are sent to the counter-party
    * @param getMutualCloseTx The Future which becomes populated with the published Mutual Close tx
    */
  def initiateMutualClose(
      dlcSetup: SetupDLC,
      sig: SchnorrDigitalSignature,
      sendSigs: (SchnorrDigitalSignature, PartialSignature) => Future[Unit],
      getMutualCloseTx: Future[Transaction]): Future[CooperativeDLCOutcome] = {
    val fundingTx = dlcSetup.fundingTx

    logger.info(s"Attempting Mutual Close for funding tx: ${fundingTx.txIdBE}")

    for {
      DLCMutualCloseSig(_, _, fundingSig) <- dlcTxSigner.createMutualCloseTxSig(
        sig)
      _ <- sendSigs(sig, fundingSig)
      mutualCloseTx <- getMutualCloseTx
    } yield {
      CooperativeDLCOutcome(fundingTx, mutualCloseTx)
    }
  }

  /** Responds to initiation of a Mutual Close by constructing and publishing the Mutual Close tx
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#mutual-closing-transaction]]
    *
    * @param getSigs The Future which becomes populated with the initiating party's signatures
    */
  def executeMutualClose(
      dlcSetup: SetupDLC,
      getSigs: Future[(SchnorrDigitalSignature, PartialSignature)]): Future[
    CooperativeDLCOutcome] = {
    val fundingTx = dlcSetup.fundingTx

    for {
      (sig, fundingSig) <- getSigs
      mutualCloseTx <- dlcTxSigner.signMutualCloseTx(sig, fundingSig)
    } yield {
      CooperativeDLCOutcome(fundingTx, mutualCloseTx)
    }
  }

  private def isToLocalOutput(output: TransactionOutput): Boolean = {
    output.scriptPubKey.isInstanceOf[P2WSHWitnessSPKV0]
  }

  /** Constructs and executes on the unilateral spending branch of a DLC
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#closing-transaction-unilateral]]
    *
    * @return Each transaction published and its spending info
    */
  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSigF: Future[SchnorrDigitalSignature]): Future[
    UnilateralDLCOutcome] = {
    val SetupDLC(fundingTx, cetInfos, _) = dlcSetup

    oracleSigF.flatMap { oracleSig =>
      val msgOpt =
        messages.find(msg => oraclePubKey.verify(msg.bytes, oracleSig))
      val (msg, cet, cetScriptWitness) = msgOpt match {
        case Some(msg) =>
          val cetInfo = cetInfos(msg)
          (msg, cetInfo.tx, cetInfo.witness)
        case None =>
          throw new IllegalArgumentException(
            s"Signature does not correspond to any possible outcome! $oracleSig")
      }

      val output = cet.outputs.head

      val privKeyWithoutTweak = oracleSig.sig.add(fundingPrivKey.fieldElement)
      val tweakHash = CryptoUtil.sha256(cetToLocalPrivKey.publicKey.bytes).flip
      val tweak = FieldElement(tweakHash.bytes)
      val privKey = privKeyWithoutTweak.add(tweak).toPrivateKey

      // Spend the true case on the correct CET
      val cetSpendingInfo = ScriptSignatureParams(
        inputInfo = P2WSHV0InputInfo(
          outPoint = TransactionOutPoint(cet.txIdBE, UInt32.zero),
          amount = output.value,
          scriptWitness = cetScriptWitness,
          conditionalPath = ConditionalPath.nonNestedTrue
        ),
        signer = privKey,
        hashType = HashType.sigHashAll
      )

      if (isToLocalOutput(output)) {
        val localSpendingTxF =
          constructClosingTx(cetSpendingInfo, msg, spendsToLocal = true)

        localSpendingTxF.map {
          case Some(localSpendingTx) =>
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
      } else {
        Future.successful(
          UnilateralDLCOutcomeWithDustClosing(fundingTx = fundingTx, cet = cet)
        )
      }
    }
  }

  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSig: SchnorrDigitalSignature): Future[UnilateralDLCOutcome] =
    executeUnilateralDLC(dlcSetup, Future.successful(oracleSig))

  /** Constructs the closing transaction on the to_remote output of a counter-party's unilateral CET broadcast
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#closing-transaction-unilateral]]
    */
  def executeRemoteUnilateralDLC(
      dlcSetup: SetupDLC,
      publishedCET: Transaction,
      sweepSPK: WitnessScriptPubKey): Future[UnilateralDLCOutcome] = {
    val output = publishedCET.outputs.last

    val spendingInfo = ScriptSignatureParams(
      inputInfo = P2WPKHV0InputInfo(
        outPoint = TransactionOutPoint(publishedCET.txIdBE, UInt32.one),
        amount = output.value,
        pubKey = finalPrivKey.publicKey),
      signer = finalPrivKey,
      hashType = HashType.sigHashAll
    )

    if (isToLocalOutput(output)) {
      Future.successful(
        UnilateralDLCOutcomeWithDustClosing(fundingTx = dlcSetup.fundingTx,
                                            cet = publishedCET)
      )
    } else {
      val msgOpt =
        dlcSetup.cets.find(_._2.remoteTxid == publishedCET.txIdBE).map(_._1)

      msgOpt match {
        case Some(msg) =>
          val txF =
            constructClosingTx(spendingInfo = spendingInfo,
                               msg = msg,
                               spendsToLocal = false,
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
              UnilateralDLCOutcomeWithDustClosing(
                fundingTx = dlcSetup.fundingTx,
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

    val (cetScriptWitness, msg) =
      dlcSetup.cets.find(_._2.remoteTxid == timedOutCET.txIdBE) match {
        case Some((msg, cetInfo)) => (cetInfo.remoteWitness, msg)
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
      signer = cetToLocalPrivKey,
      hashType = HashType.sigHashAll
    )

    if (isToLocalOutput(justiceOutput)) {
      val justiceSpendingTxF =
        constructClosingTx(spendingInfo = justiceSpendingInfo,
                           msg = msg,
                           spendsToLocal = false)

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
    } else {
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
        pubKey = finalPrivKey.publicKey
      ),
      signer = finalPrivKey,
      hashType = HashType.sigHashAll
    )

    val localSpendingTxF = constructClosingTx(
      localRefundSpendingInfo,
      msg = Sha256DigestBE(ByteVector.fill(32)(0.toByte)), // Not used
      spendsToLocal = false)

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
}

object DLCClient {

  def apply(
      outcomes: ContractInfo,
      oraclePubKey: SchnorrPublicKey,
      preCommittedR: SchnorrNonce,
      isInitiator: Boolean,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      remotePubKeys: DLCPublicKeys,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]],
      remoteFundingInputs: Vector[OutputReference],
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerVirtualByte,
      changeSPK: ScriptPubKey,
      remoteChangeSPK: ScriptPubKey,
      network: BitcoinNetwork)(implicit ec: ExecutionContext): DLCClient = {
    val pubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(extPrivKey,
                                                       nextAddressIndex,
                                                       network)

    val remoteOutcomes: ContractInfo = ContractInfo(outcomes.map {
      case (hash, amt) => (hash, (input + remoteInput - amt).satoshis)
    })

    val changeAddress = BitcoinAddress.fromScriptPubKey(changeSPK, network).get
    val remoteChangeAddress =
      BitcoinAddress.fromScriptPubKey(remoteChangeSPK, network).get

    val (offerOutcomes,
         offerPubKeys,
         offerInput,
         offerFundingInputs,
         offerChangeAddress,
         acceptPubKeys,
         acceptInput,
         acceptFundingInputs,
         acceptChangeAddress) = if (isInitiator) {
      (outcomes,
       pubKeys,
       input,
       fundingUtxos.map(_.outputReference),
       changeAddress,
       remotePubKeys,
       remoteInput,
       remoteFundingInputs,
       remoteChangeAddress)
    } else {
      (remoteOutcomes,
       remotePubKeys,
       remoteInput,
       remoteFundingInputs,
       remoteChangeAddress,
       pubKeys,
       input,
       fundingUtxos.map(_.outputReference),
       changeAddress)
    }

    val offer = DLCMessage.DLCOffer(
      contractInfo = offerOutcomes,
      oracleInfo = DLCMessage.OracleInfo(oraclePubKey, preCommittedR),
      pubKeys = offerPubKeys,
      totalCollateral = offerInput.satoshis,
      fundingInputs = offerFundingInputs,
      changeAddress = offerChangeAddress,
      feeRate = feeRate,
      timeouts = timeouts
    )

    val accept = DLCMessage.DLCAcceptWithoutSigs(
      totalCollateral = acceptInput.satoshis,
      pubKeys = acceptPubKeys,
      fundingInputs = acceptFundingInputs,
      changeAddress = acceptChangeAddress,
      eventId = offer.eventId)

    DLCClient(offer,
              accept,
              isInitiator,
              extPrivKey,
              nextAddressIndex,
              fundingUtxos)
  }

  def apply(
      outcomeWin: String,
      outcomeLose: String,
      oraclePubKey: SchnorrPublicKey,
      preCommittedR: SchnorrNonce,
      isInitiator: Boolean,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      remotePubKeys: DLCPublicKeys,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]],
      remoteFundingInputs: Vector[OutputReference],
      winPayout: CurrencyUnit,
      losePayout: CurrencyUnit,
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerVirtualByte,
      changeSPK: WitnessScriptPubKeyV0,
      remoteChangeSPK: WitnessScriptPubKeyV0,
      network: BitcoinNetwork)(implicit ec: ExecutionContext): DLCClient = {
    val hashWin = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
    val hashLose = CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip

    val outcomes = ContractInfo(
      Map(
        hashWin -> winPayout.satoshis,
        hashLose -> losePayout.satoshis
      )
    )

    DLCClient(
      outcomes = outcomes,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      isInitiator = isInitiator,
      extPrivKey = extPrivKey,
      nextAddressIndex = nextAddressIndex,
      remotePubKeys = remotePubKeys,
      input = input,
      remoteInput = remoteInput,
      fundingUtxos = fundingUtxos,
      remoteFundingInputs = remoteFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = changeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = network
    )
  }

  def fromOffer(
      offer: DLCMessage.DLCOffer,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]],
      totalCollateral: CurrencyUnit,
      changeSPK: P2WPKHWitnessSPKV0,
      network: BitcoinNetwork)(implicit ec: ExecutionContext): DLCClient = {
    val accept = DLCMessage.DLCAcceptWithoutSigs(
      totalCollateral.satoshis,
      DLCPublicKeys.fromExtPrivKeyAndIndex(extPrivKey,
                                           nextAddressIndex,
                                           network),
      fundingUtxos.map(_.outputReference),
      Bech32Address(changeSPK, network),
      offer.eventId
    )

    DLCClient(offer = offer,
              accept = accept,
              isInitiator = false,
              extPrivKey = extPrivKey,
              nextAddressIndex = nextAddressIndex,
              fundingUtxos = fundingUtxos)
  }

  def fromOfferAndAccept(
      offer: DLCMessage.DLCOffer,
      accept: DLCMessage.DLCAccept,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(
      implicit ec: ExecutionContext): DLCClient = {
    val acceptWithoutSigs = DLCMessage.DLCAcceptWithoutSigs(
      accept.totalCollateral,
      accept.pubKeys,
      accept.fundingInputs,
      accept.changeAddress,
      accept.eventId)

    DLCClient(offer = offer,
              accept = acceptWithoutSigs,
              isInitiator = true,
              extPrivKey = extPrivKey,
              nextAddressIndex = nextAddressIndex,
              fundingUtxos = fundingUtxos)
  }
}
