package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCMutualCloseSig
}
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.{BitcoinTxBuilder, TxBuilder}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** This case class allows for the construction and execution of
  * Discreet Log Contracts between two parties.
  *
  * @param outcomes The ContractInfo from the local view
  * @param oraclePubKey The Oracle's permanent public key
  * @param preCommittedR The Oracle's one-time event-specific public key
  * @param isInitiator True if this client sends the offer message
  * @param extPrivKey This client's extended private key (at the account level) for this event
  * @param nextAddressIndex The next unused address index for the provided extPrivKey
  * @param remotePubKeys Remote's public keys for this event
  * @param input This client's total collateral contribution
  * @param remoteInput Remote's total collateral contribution
  * @param fundingUtxos This client's funding BitcoinUTXOSpendingInfo collection
  * @param remoteFundingInputs Remote's funding outpoints and their outputs
  * @param timeouts The timeouts for this DLC
  * @param feeRate The predicted fee rate used for all transactions
  * @param changeSPK This client's change ScriptPubKey used in the funding tx
  * @param remoteChangeSPK Remote's change ScriptPubKey used in the funding tx
  */
case class DLCClient(
    outcomes: ContractInfo,
    oraclePubKey: SchnorrPublicKey,
    preCommittedR: SchnorrNonce,
    isInitiator: Boolean,
    extPrivKey: ExtPrivateKey,
    nextAddressIndex: Int,
    remotePubKeys: DLCPublicKeys,
    input: CurrencyUnit,
    remoteInput: CurrencyUnit,
    fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
    remoteFundingInputs: Vector[OutputReference],
    timeouts: DLCTimeouts,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    remoteChangeSPK: ScriptPubKey,
    network: BitcoinNetwork)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  import DLCClient._

  private val sigPubKeys = outcomes.keys.map { msg =>
    msg -> oraclePubKey.computeSigPoint(msg.bytes, preCommittedR)
  }.toMap

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

  /** Total collateral amount */
  private val totalInput = input + remoteInput

  private val totalFunding =
    fundingUtxos.foldLeft(0L)(_ + _.amount.satoshis.toLong)
  private val remoteTotalFunding =
    remoteFundingInputs.foldLeft(0L) {
      case (accum, OutputReference(_, output)) =>
        accum + output.value.satoshis.toLong
    }

  val remoteOutcomes: ContractInfo = ContractInfo(outcomes.map {
    case (hash, amt) => (hash, (totalInput - amt).satoshis)
  })

  /** This is only used as a placeholder and we use an invariant
    * when signing to ensure that this is never used.
    *
    * In the future, allowing this behavior should be done in TxBuilder.
    */
  private val emptyChangeSPK: ScriptPubKey = EmptyScriptPubKey

  val fundingPubKey: ECPublicKey = fundingPrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    val fundingKeys = if (isInitiator) {
      Vector(fundingPubKey, fundingRemotePubKey)
    } else {
      Vector(fundingRemotePubKey, fundingPubKey)
    }

    MultiSignatureScriptPubKey(2, fundingKeys)
  }

  /** Experimental approx. vbytes for a CET */
  private val approxCETVBytes = 190

  /** Experimental approx. vbytes for a closing tx spending ToLocalOutput */
  private val approxToLocalClosingVBytes = 122

  private val cetFee: CurrencyUnit = Satoshis(approxCETVBytes * feeRate.toLong)
  private val toLocalClosingFee: CurrencyUnit = Satoshis(
    approxToLocalClosingVBytes * feeRate.toLong)

  private val isRBFEnabled = false
  private val (noTimeLockSequence, timeLockSequence) =
    if (isRBFEnabled) (UInt32.zero, UInt32.zero)
    else
      (TransactionConstants.sequence, TransactionConstants.disableRBFSequence)

  lazy val createUnsignedFundingTransaction: Transaction = {
    /* We need to commit to the CET's and local closing tx's fee during the construction of
     * the funding transaction so that the CET outputs have the expected payouts.
     *
     * Once computed, we add the estimated amount to the fundingOutput so it can be used for fees later.
     */
    val halfCetFee = Satoshis((cetFee + toLocalClosingFee).satoshis.toLong / 2)

    val output: TransactionOutput =
      TransactionOutput(totalInput + halfCetFee + halfCetFee,
                        P2WSHWitnessSPKV0(fundingSPK))

    val (initiatorChange, initiatorChangeSPK, otherChange, otherChangeSPK) =
      if (isInitiator) {
        (Satoshis(totalFunding) - input,
         changeSPK,
         Satoshis(remoteTotalFunding) - remoteInput,
         remoteChangeSPK)
      } else {
        (Satoshis(remoteTotalFunding) - remoteInput,
         remoteChangeSPK,
         Satoshis(totalFunding) - input,
         changeSPK)
      }

    val initiatorChangeOutput =
      TransactionOutput(initiatorChange - halfCetFee, initiatorChangeSPK)
    val otherChangeOutput =
      TransactionOutput(otherChange - halfCetFee, otherChangeSPK)

    val outputs: Vector[TransactionOutput] =
      Vector(output, initiatorChangeOutput, otherChangeOutput)

    val localInputs =
      TxBuilder.calcSequenceForInputs(fundingUtxos, noTimeLockSequence)
    val remoteInputs = remoteFundingInputs.map {
      case OutputReference(outPoint, _) =>
        TransactionInput(outPoint, EmptyScriptSignature, noTimeLockSequence)
    }
    val inputs = if (isInitiator) {
      localInputs ++ remoteInputs
    } else {
      remoteInputs ++ localInputs
    }

    val txWithoutFee = BaseTransaction(version =
                                         TransactionConstants.validLockVersion,
                                       inputs = inputs,
                                       outputs = outputs,
                                       lockTime = UInt32.zero)

    val txWithFee = subtractFeeFromOutputs(txWithoutFee,
                                           feeRate,
                                           Vector(changeSPK, remoteChangeSPK))

    BaseTransaction(version = txWithFee.version,
                    inputs = txWithFee.inputs,
                    outputs =
                      txWithFee.outputs.filter(_.value >= Policy.dustThreshold),
                    lockTime = txWithFee.lockTime)
  }

  lazy val fundingTxId: DoubleSha256Digest =
    createUnsignedFundingTransaction.txId

  lazy val fundingOutput: TransactionOutput =
    createUnsignedFundingTransaction.outputs.head

  /** Returns the payouts for the signature as (toLocal, toRemote)  */
  def getPayouts(
      oracleSig: SchnorrDigitalSignature): (CurrencyUnit, CurrencyUnit) = {
    sigPubKeys.find(_._2 == oracleSig.sig.getPublicKey) match {
      case Some((hash, _)) =>
        (outcomes(hash), remoteOutcomes(hash))
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to a possible outcome! $oracleSig")
    }
  }

  def createFundingTransactionSigs(): Future[FundingSignatures] = {
    val fundingTx = createUnsignedFundingTransaction

    val outPointAndSigFs = fundingUtxos.foldLeft(
      Vector.empty[Future[(TransactionOutPoint, PartialSignature)]]) {
      case (vec, utxo) =>
        val sigF = BitcoinSigner
          .signSingle(utxo, fundingTx, isDummySignature = false)
          .map(sig => (utxo.outPoint, sig))
        vec :+ sigF
    }
    val sigsF = Future.sequence(outPointAndSigFs)

    val sigsByOutPointF = sigsF.map(_.groupBy(_._1))
    val sigsMapF = sigsByOutPointF.map(_.map {
      case (outPoint, outPointAndSigs) =>
        outPoint -> outPointAndSigs.map(_._2)
    })

    sigsMapF.map(FundingSignatures.apply)
  }

  def createFundingTransaction(
      remoteSigs: FundingSignatures): Future[Transaction] = {
    val (localTweak, remoteTweak) = if (isInitiator) {
      (0, fundingUtxos.length)
    } else {
      (remoteFundingInputs.length, 0)
    }

    val fundingPSBT = remoteSigs.zipWithIndex
      .foldLeft(PSBT.fromUnsignedTx(createUnsignedFundingTransaction)) {
        case (psbt, ((outPoint, sigs), index)) =>
          require(psbt.transaction.inputs(index).previousOutput == outPoint,
                  "Adding signature for incorrect input")
          psbt
            .addWitnessUTXOToInput(remoteFundingInputs(index).output,
                                   index + remoteTweak)
            .addSignatures(sigs, index + remoteTweak)
      }

    val signedFundingPSBTF =
      fundingUtxos.zipWithIndex.foldLeft(Future.successful(fundingPSBT)) {
        case (psbtF, (utxo, index)) =>
          psbtF.flatMap { psbt =>
            psbt
              .addWitnessUTXOToInput(output = utxo.output, index + localTweak)
              .addScriptWitnessToInput(
                scriptWitness = utxo.scriptWitnessOpt.get,
                index + localTweak)
              .sign(index + localTweak, utxo.signer)
          }
      }

    signedFundingPSBTF.flatMap { signedFundingPSBT =>
      val txT =
        signedFundingPSBT.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      Future.fromTry(txT)
    }
  }

  /** Creates a ready-to-sign PSBT for the Mutual Close tx
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#mutual-closing-transaction]]
    *
    * @param sig The oracle's signature for this contract
    */
  def createUnsignedMutualClosePSBT(sig: SchnorrDigitalSignature): PSBT = {
    val (toLocalPayout, toRemotePayout) = getPayouts(sig)

    val toLocal =
      TransactionOutput(toLocalPayout,
                        P2WPKHWitnessSPKV0(finalPrivKey.publicKey))
    val toRemote =
      TransactionOutput(toRemotePayout, finalRemoteScriptPubKey)
    val outputs = if (isInitiator) {
      Vector(toLocal, toRemote)
    } else {
      Vector(toRemote, toLocal)
    }
    val input = TransactionInput(TransactionOutPoint(fundingTxId, UInt32.zero),
                                 EmptyScriptSignature,
                                 noTimeLockSequence)
    val utx = BaseTransaction(TransactionConstants.validLockVersion,
                              Vector(input),
                              outputs.filter(_.value >= Policy.dustThreshold),
                              UInt32.zero)

    PSBT
      .fromUnsignedTx(utx)
      .addWitnessUTXOToInput(fundingOutput, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
  }

  def createMutualCloseTxSig(
      sig: SchnorrDigitalSignature): Future[PartialSignature] = {
    val unsignedPSBT =
      createUnsignedMutualClosePSBT(sig)

    unsignedPSBT
      .sign(inputIndex = 0, fundingPrivKey)
      .flatMap(findSigInPSBT(_, fundingPrivKey.publicKey))
  }

  def createMutualCloseTx(
      sig: SchnorrDigitalSignature,
      fundingSig: PartialSignature): Future[Transaction] = {
    val unsignedPSBT =
      createUnsignedMutualClosePSBT(sig)

    val signedPSBTF = unsignedPSBT
      .addSignature(fundingSig, inputIndex = 0)
      .sign(inputIndex = 0, fundingPrivKey)

    signedPSBTF.flatMap { signedPSBT =>
      val txT = signedPSBT.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      Future.fromTry(txT)
    }
  }

  /** Constructs CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCET(
      msg: Sha256DigestBE,
      remoteSig: PartialSignature): (Future[Transaction], P2WSHWitnessV0) = {
    val tweak = CryptoUtil.sha256(cetToLocalPrivKey.publicKey.bytes).flip
    val tweakPubKey = ECPrivateKey.fromBytes(tweak.bytes).publicKey

    val pubKey = sigPubKeys(msg).add(fundingPubKey).add(tweakPubKey)

    val toLocalSPK = P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout.toLong),
      timeoutPubKey = cetToLocalRemotePubKey
    )

    val toLocal: TransactionOutput =
      TransactionOutput(outcomes(msg) + toLocalClosingFee,
                        P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(remoteOutcomes(msg), finalRemoteScriptPubKey)

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)

    val fundingInput = TransactionInput(
      TransactionOutPoint(fundingTxId, UInt32.zero),
      EmptyScriptSignature,
      timeLockSequence)

    val psbt = PSBT.fromUnsignedTx(
      BaseTransaction(TransactionConstants.validLockVersion,
                      Vector(fundingInput),
                      outputs.filter(_.value >= Policy.dustThreshold),
                      timeouts.contractMaturity.toUInt32)
    )

    val readyToSignPSBT = psbt
      .addSignature(remoteSig, inputIndex = 0)
      .addWitnessUTXOToInput(fundingOutput, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)

    val signedPSBTF = readyToSignPSBT.sign(inputIndex = 0, fundingPrivKey)

    val signedCETF = signedPSBTF.flatMap { signedPSBT =>
      val txT = signedPSBT.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      Future.fromTry(txT)
    }

    (signedCETF, P2WSHWitnessV0(toLocalSPK))
  }

  private def findSigInPSBT(
      psbt: PSBT,
      pubKey: ECPublicKey): Future[PartialSignature] = {
    val sigOpt = psbt.inputMaps.head.partialSignatures
      .find(_.pubKey == pubKey)

    sigOpt match {
      case None =>
        Future.failed(new RuntimeException("No signature found after signing"))
      case Some(partialSig) => Future.successful(partialSig)
    }
  }

  /** Constructs Remote's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETRemote(msg: Sha256DigestBE): Future[
    (Transaction, P2WSHWitnessV0, PartialSignature)] = {
    val tweak = CryptoUtil.sha256(cetToLocalRemotePubKey.bytes).flip
    val tweakPubKey = ECPrivateKey.fromBytes(tweak.bytes).publicKey

    val pubKey = sigPubKeys(msg).add(fundingRemotePubKey).add(tweakPubKey)

    val toLocalSPK = P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout.toLong),
      timeoutPubKey = cetToLocalPrivKey.publicKey
    )

    val toLocal: TransactionOutput =
      TransactionOutput(remoteOutcomes(msg) + toLocalClosingFee,
                        P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(outcomes(msg),
                        P2WPKHWitnessSPKV0(finalPrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)

    val fundingOutPoint = TransactionOutPoint(fundingTxId, UInt32.zero)
    val fundingInput =
      TransactionInput(fundingOutPoint, EmptyScriptSignature, timeLockSequence)

    val unsignedTx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector(fundingInput),
      outputs.filter(_.value >= Policy.dustThreshold),
      timeouts.contractMaturity.toUInt32)

    val sigF = PSBT
      .fromUnsignedTx(unsignedTx)
      .addWitnessUTXOToInput(fundingOutput, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      .sign(inputIndex = 0, fundingPrivKey)
      .flatMap(findSigInPSBT(_, fundingPrivKey.publicKey))

    sigF.map((unsignedTx, P2WSHWitnessV0(toLocalSPK), _))
  }

  lazy val createUnsignedRefundTx: Transaction = {
    val fundingTx = createUnsignedFundingTransaction
    val fundingTxid = fundingTx.txId
    val fundingInput = TransactionInput(
      TransactionOutPoint(fundingTxid, UInt32.zero),
      EmptyScriptSignature,
      timeLockSequence)
    val fundingOutput = fundingTx.outputs.head

    val (initiatorValue, initiatorSPK, otherValue, otherSPK) =
      if (isInitiator) {
        val toLocalValue = Satoshis(
          (fundingOutput.value * input).satoshis.toLong / totalInput.satoshis.toLong)

        (toLocalValue,
         P2WPKHWitnessSPKV0(finalPrivKey.publicKey),
         fundingOutput.value - toLocalValue,
         finalRemoteScriptPubKey)
      } else {
        val toRemoteValue = Satoshis(
          (fundingOutput.value * remoteInput).satoshis.toLong / totalInput.satoshis.toLong)

        (toRemoteValue,
         finalRemoteScriptPubKey,
         fundingOutput.value - toRemoteValue,
         P2WPKHWitnessSPKV0(finalPrivKey.publicKey))
      }

    val toInitiatorOutput =
      TransactionOutput(initiatorValue, initiatorSPK)
    val toOtherOutput =
      TransactionOutput(otherValue, otherSPK)

    val outputs = Vector(toInitiatorOutput, toOtherOutput)

    val refundTxNoFee = BaseTransaction(TransactionConstants.validLockVersion,
                                        Vector(fundingInput),
                                        outputs,
                                        timeouts.contractTimeout.toUInt32)

    subtractFeeFromOutputs(refundTxNoFee, feeRate, outputs.map(_.scriptPubKey))
  }

  def createRefundSig(): Future[(Transaction, PartialSignature)] = {
    val refundTx = createUnsignedRefundTx

    val sigF = PSBT
      .fromUnsignedTx(refundTx)
      .addWitnessUTXOToInput(fundingOutput, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      .sign(inputIndex = 0, fundingPrivKey)
      .flatMap(findSigInPSBT(_, fundingPrivKey.publicKey))

    sigF.map((refundTx, _))
  }

  /** Constructs the (time-locked) refund transaction for when the oracle disappears
    * or signs an unknown message.
    * Note that both parties have the same refund transaction.
    */
  def createRefundTx(
      remoteSig: PartialSignature): Future[(Transaction, PartialSignature)] = {
    val psbt = PSBT.fromUnsignedTx(createUnsignedRefundTx)

    val signedPSBTF = psbt
      .addSignature(remoteSig, inputIndex = 0)
      .addWitnessUTXOToInput(fundingOutput, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      .sign(inputIndex = 0, fundingPrivKey)

    signedPSBTF.flatMap { signedPSBT =>
      val sigF = findSigInPSBT(signedPSBT, fundingPrivKey.publicKey)

      val txT = signedPSBT.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      val txF = Future.fromTry(txT)
      txF.flatMap(tx => sigF.map(sig => (tx, sig)))
    }
  }

  def createCETSigs: Future[CETSignatures] = {
    val cetDataFs = outcomes.keys.map { msg =>
      createCETRemote(msg).map(msg -> _)
    }
    for {
      cetData <- Future.sequence(cetDataFs).map(_.toMap)
      sigs = cetData.map { case (msg, (_, _, sig)) => msg -> sig }
      (_, remoteRefundSig) <- createRefundSig()
    } yield {
      CETSignatures(sigs, remoteRefundSig)
    }
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

    val remoteCetDataFs = outcomes.keys.map { msg =>
      createCETRemote(msg).map(msg -> _)
    }

    for {
      remoteCetData <- Future.sequence(remoteCetDataFs).map(_.toMap)
      (_, remoteRefundSig) <- createRefundSig()
      remoteOutcomeSigs = remoteCetData.map {
        case (msg, (_, _, sig)) => msg -> sig
      }
      remoteCetSigs = CETSignatures(remoteOutcomeSigs, remoteRefundSig)
      _ <- sendSigs(remoteCetSigs)
      (cetSigs, fundingSigs) <- getSigs

      localCetDataFs = cetSigs.outcomeSigs.map {
        case (msg, sig) =>
          val (txF, witness) = createCET(msg, sig)
          txF.map(msg -> (_, witness))
      }

      localCetData <- Future.sequence(localCetDataFs).map(_.toMap)
      (refundTx, _) <- createRefundTx(cetSigs.refundSig)
      fundingTx <- createFundingTransaction(fundingSigs)
    } yield {
      val cetInfos = outcomes.keys.map { msg =>
        val (localCet, localWitness) = localCetData(msg)
        val (remoteCet, remoteWitness, _) = remoteCetData(msg)
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
            val (txF, witness) = createCET(msg, sig)
            txF.map(msg -> (_, witness))
        }
        val remoteCetDataFs = outcomes.keys.map { msg =>
          createCETRemote(msg).map(msg -> _)
        }
        val refundTxF = createRefundTx(refundSig)

        for {
          localCetData <- Future.sequence(localCetDataFs).map(_.toMap)
          remoteCetData <- Future.sequence(remoteCetDataFs).map(_.toMap)
          (refundTx, remoteRefundSig) <- refundTxF
          remoteOutcomeSigs = remoteCetData.map {
            case (msg, (_, _, sig)) => msg -> sig
          }
          cetSigs = CETSignatures(remoteOutcomeSigs, remoteRefundSig)
          localFundingSigs <- createFundingTransactionSigs()
          _ <- sendSigs(cetSigs, localFundingSigs)
          fundingTx <- getFundingTx
        } yield {
          val cetInfos = outcomes.keys.map { msg =>
            val (localCet, localWitness) = localCetData(msg)
            val (remoteCet, remoteWitness, _) = remoteCetData(msg)
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
      spendingInfo: BitcoinUTXOSpendingInfoFull,
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

        val txBuilder = BitcoinTxBuilder(
          destinations = Vector(TransactionOutput(payoutValue, sweepSPK)),
          utxos = Vector(spendingInfo),
          feeRate = feeRate,
          changeSPK = emptyChangeSPK,
          network = network
        )

        txBuilder.flatMap(_.sign).map(Some(_))
      }
    } else {
      val txBuilder = BitcoinTxBuilder(
        destinations =
          Vector(TransactionOutput(spendingInfo.output.value, sweepSPK)),
        utxos = Vector(spendingInfo),
        feeRate = feeRate,
        changeSPK = emptyChangeSPK,
        network = network
      )

      txBuilder.flatMap(subtractFeeAndSign).map(Some(_))
    }

    spendingTxOptF.foreach(txOpt =>
      logger.info(s"Closing Tx: ${txOpt.map(_.hex)}"))

    spendingTxOptF
  }

  def createMutualCloseSig(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {

    createMutualCloseTxSig(oracleSig).map { sig =>
      DLCMutualCloseSig(eventId, oracleSig, sig)
    }
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
      fundingSig <- createMutualCloseTxSig(sig)
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
      mutualCloseTx <- createMutualCloseTx(sig, fundingSig)
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
        outcomes.keys.find(msg => oraclePubKey.verify(msg.bytes, oracleSig))
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
      val cetSpendingInfo = P2WSHV0SpendingInfoFull(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.zero),
        amount = output.value,
        scriptPubKey = P2WSHWitnessSPKV0(cetScriptWitness.redeemScript),
        signersWithPossibleExtra = Vector(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = cetScriptWitness,
        conditionalPath = ConditionalPath.nonNestedTrue
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

    val spendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(publishedCET.txIdBE, UInt32.one),
      amount = output.value,
      scriptPubKey = P2WPKHWitnessSPKV0(finalPrivKey.publicKey),
      signer = finalPrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(finalPrivKey.publicKey)
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

    val justiceSpendingInfo = P2WSHV0SpendingInfoFull(
      outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.zero),
      amount = justiceOutput.value,
      scriptPubKey = P2WSHWitnessSPKV0(cetScriptWitness.redeemScript),
      signersWithPossibleExtra = Vector(cetToLocalPrivKey),
      hashType = HashType.sigHashAll,
      scriptWitness = cetScriptWitness,
      conditionalPath = ConditionalPath.nonNestedFalse
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

    val localRefundSpendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, vout),
      amount = localOutput.value,
      scriptPubKey = P2WPKHWitnessSPKV0(finalPrivKey.publicKey),
      signer = finalPrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(finalPrivKey.publicKey)
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
      fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
      remoteFundingInputs: Vector[OutputReference],
      winPayout: CurrencyUnit,
      losePayout: CurrencyUnit,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
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
      fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
      totalCollateral: CurrencyUnit,
      changeSPK: P2WPKHWitnessSPKV0,
      network: BitcoinNetwork)(implicit ec: ExecutionContext): DLCClient = {
    val outcomes: ContractInfo = ContractInfo(offer.contractInfo.map {
      case (hash, amt) =>
        (hash, (totalCollateral + offer.totalCollateral - amt).satoshis)
    })

    DLCClient(
      outcomes = outcomes,
      oraclePubKey = offer.oracleInfo.pubKey,
      preCommittedR = offer.oracleInfo.rValue,
      isInitiator = false,
      extPrivKey = extPrivKey,
      nextAddressIndex = nextAddressIndex,
      remotePubKeys = offer.pubKeys,
      input = totalCollateral,
      remoteInput = offer.totalCollateral,
      fundingUtxos = fundingUtxos,
      remoteFundingInputs = offer.fundingInputs,
      timeouts = offer.timeouts,
      feeRate = offer.feeRate,
      changeSPK = changeSPK,
      remoteChangeSPK =
        offer.changeAddress.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0],
      network = network
    )
  }

  def fromOfferAndAccept(
      offer: DLCMessage.DLCOffer,
      accept: DLCMessage.DLCAccept,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
      network: BitcoinNetwork)(implicit ec: ExecutionContext): DLCClient = {
    val pubKeys = DLCPublicKeys
      .fromExtPrivKeyAndIndex(extPrivKey, nextAddressIndex, network)
    require(
      pubKeys == offer.pubKeys,
      s"ExtPrivateKey must match the one in your Offer message: ${offer.pubKeys}, got: $pubKeys")
    require(
      fundingUtxos.zip(offer.fundingInputs).forall {
        case (info, OutputReference(outPoint, output)) =>
          info.output == output && info.outPoint == outPoint
      },
      s"Funding UTXOs must match those in your Offer message: ${offer.fundingInputs}, got: ${fundingUtxos
        .map(utxo => OutputReference(utxo.outPoint, utxo.output))}"
    )

    DLCClient(
      outcomes = offer.contractInfo,
      oraclePubKey = offer.oracleInfo.pubKey,
      preCommittedR = offer.oracleInfo.rValue,
      isInitiator = true,
      extPrivKey = extPrivKey,
      nextAddressIndex = nextAddressIndex,
      remotePubKeys = accept.pubKeys,
      input = offer.totalCollateral,
      remoteInput = accept.totalCollateral,
      fundingUtxos = fundingUtxos,
      remoteFundingInputs = accept.fundingInputs,
      timeouts = offer.timeouts,
      feeRate = offer.feeRate,
      changeSPK =
        offer.changeAddress.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0],
      remoteChangeSPK =
        accept.changeAddress.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0],
      network = network
    )
  }

  /** Subtracts the estimated fee by removing from each output evenly */
  def subtractFeeAndSign(txBuilder: BitcoinTxBuilder)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val spks = txBuilder.destinations.toVector.map(_.scriptPubKey)

    subtractFeeFromOutputsAndSign(txBuilder, spks)
  }

  // This invariant ensures that emptyChangeSPK is never used above
  val noEmptyOutputs: (Seq[BitcoinUTXOSpendingInfoFull], Transaction) => Boolean = {
    (_, tx) =>
      tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  /** Subtracts the estimated fee by removing from each output with a specified spk evenly */
  def subtractFeeFromOutputsAndSign(
      txBuilder: BitcoinTxBuilder,
      spks: Vector[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[Transaction] = {
    subtractFeeFromOutputs(txBuilder, spks).flatMap(_.sign(noEmptyOutputs))
  }

  private def subtractFees(
      tx: Transaction,
      feeRate: FeeUnit,
      spks: Vector[ScriptPubKey]): Vector[TransactionOutput] = {
    val fee = feeRate.calc(tx)

    val outputs = tx.outputs.zipWithIndex.filter {
      case (output, _) => spks.contains(output.scriptPubKey)
    }
    val unchangedOutputs = tx.outputs.zipWithIndex.filterNot {
      case (output, _) => spks.contains(output.scriptPubKey)
    }

    val feePerOutput = Satoshis(Int64(fee.satoshis.toLong / outputs.length))
    val feeRemainder = Satoshis(Int64(fee.satoshis.toLong % outputs.length))

    val newOutputsWithoutRemainder = outputs.map {
      case (output, index) =>
        (TransactionOutput(output.value - feePerOutput, output.scriptPubKey),
         index)
    }
    val (lastOutput, lastOutputIndex) = newOutputsWithoutRemainder.last
    val newLastOutput = TransactionOutput(lastOutput.value - feeRemainder,
                                          lastOutput.scriptPubKey)
    val newOutputs = newOutputsWithoutRemainder
      .dropRight(1)
      .:+((newLastOutput, lastOutputIndex))

    (newOutputs ++ unchangedOutputs).sortBy(_._2).map(_._1).toVector
  }

  /** Subtracts the estimated fee by removing from each output with a specified spk evenly */
  def subtractFeeFromOutputs(
      txBuilder: BitcoinTxBuilder,
      spks: Vector[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[BitcoinTxBuilder] = {
    txBuilder.unsignedTx.flatMap { tx =>
      val allOuputsWithNew = subtractFees(tx, txBuilder.feeRate, spks)

      BitcoinTxBuilder(allOuputsWithNew,
                       txBuilder.utxoMap,
                       txBuilder.feeRate,
                       txBuilder.changeSPK,
                       txBuilder.network,
                       txBuilder.lockTimeOverrideOpt)
    }
  }

  /** Subtracts the estimated fee by removing from each output with a specified spk evenly */
  def subtractFeeFromOutputs(
      tx: BaseTransaction,
      feeRate: FeeUnit,
      spks: Vector[ScriptPubKey]): BaseTransaction = {
    // tx has empty script sigs and we need to account for witness data in fees
    val dummyTxWit = TransactionWitness(
      Vector.fill(tx.inputs.length)(
        P2WPKHWitnessV0(ECPublicKey.freshPublicKey, DummyECDigitalSignature)))
    val wtx = WitnessTransaction(tx.version,
                                 tx.inputs,
                                 tx.outputs,
                                 tx.lockTime,
                                 dummyTxWit)

    val allOuputsWithNew = subtractFees(wtx, feeRate, spks)

    BaseTransaction(tx.version, tx.inputs, allOuputsWithNew, tx.lockTime)
  }
}
