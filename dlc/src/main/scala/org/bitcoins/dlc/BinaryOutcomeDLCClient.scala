package org.bitcoins.dlc

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  DummyECDigitalSignature,
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrDigitalSignature,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2PKWithTimeoutScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  ScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput,
  TransactionWitness,
  WitnessTransaction
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil}
import org.bitcoins.core.wallet.builder.{BitcoinTxBuilder, TxBuilder}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSignerSingle
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
  BitcoinUTXOSpendingInfoSingle,
  ConditionalPath,
  P2WPKHV0SpendingInfo,
  P2WSHV0SpendingInfoFull
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** This case class allows for the construction and execution of binary outcome
  * Discreet Log Contracts between two parties.
  *
  * @param outcomeWin The String whose hash is signed by the oracle in the Win case
  * @param outcomeLose The String whose hash is signed by the oracle in the Lose case
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
  * @param winPayout This client's payout in the Win case
  * @param losePayout This client's payout in the Lose case
  * @param timeouts The timeouts for this DLC
  * @param feeRate The predicted fee rate used for all transactions
  * @param changeSPK This client's change ScriptPubKey used in the funding tx
  * @param remoteChangeSPK Remote's change ScriptPubKey used in the funding tx
  */
case class BinaryOutcomeDLCClient(
    outcomeWin: Sha256DigestBE,
    outcomeLose: Sha256DigestBE,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey,
    isInitiator: Boolean,
    extPrivKey: ExtPrivateKey,
    nextAddressIndex: Int,
    remotePubKeys: DLCPublicKeys,
    input: CurrencyUnit,
    remoteInput: CurrencyUnit,
    fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
    remoteFundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
    winPayout: CurrencyUnit,
    losePayout: CurrencyUnit,
    timeouts: DLCTimeouts,
    feeRate: FeeUnit,
    changeSPK: WitnessScriptPubKeyV0,
    remoteChangeSPK: WitnessScriptPubKeyV0,
    network: BitcoinNetwork)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  import BinaryOutcomeDLCClient._

  /** Hash signed by oracle in Win case */
  val messageWin: ByteVector = outcomeWin.bytes

  /** Hash signed by oracle in Lose case */
  val messageLose: ByteVector = outcomeLose.bytes

  /** sig*G in the Win case */
  val sigPubKeyWin: ECPublicKey =
    Schnorr.computePubKey(messageWin, preCommittedR, oraclePubKey)

  /** sig*G in the Lose case */
  val sigPubKeyLose: ECPublicKey =
    Schnorr.computePubKey(messageLose, preCommittedR, oraclePubKey)

  val fundingPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/$nextAddressIndex"))
      .key

  val cetToLocalPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 1}"))
      .key

  val cetToRemotePrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 2}"))
      .key

  val finalPrivKey: ECPrivateKey =
    extPrivKey
      .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 3}"))
      .key

  val fundingRemotePubKey: ECPublicKey = remotePubKeys.fundingKey

  val cetToLocalRemotePubKey: ECPublicKey = remotePubKeys.toLocalCETKey

  val cetToRemoteRemotePubKey: ECPublicKey = remotePubKeys.toRemoteCETKey

  val finalRemoteScriptPubKey: ScriptPubKey =
    remotePubKeys.finalAddress.scriptPubKey

  /** Total collateral amount */
  private val totalInput = input + remoteInput

  private val totalFunding =
    fundingUtxos.foldLeft(0L)(_ + _.amount.satoshis.toLong)
  private val remoteTotalFunding =
    remoteFundingInputs.foldLeft(0L) {
      case (accum, (_, output)) =>
        accum + output.value.satoshis.toLong
    }

  /** Remote's payout in the Win case (in which Remote loses) */
  val remoteWinPayout: CurrencyUnit = totalInput - winPayout

  /** Remote's payout in the Lose case (in which Remote wins) */
  val remoteLosePayout: CurrencyUnit = totalInput - losePayout

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
  private val sequence =
    if (isRBFEnabled) UInt32.zero else TransactionConstants.disableRBFSequence

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
      TxBuilder.calcSequenceForInputs(fundingUtxos, sequence)
    val remoteInputs = remoteFundingInputs.map {
      case (outPoint, _) =>
        TransactionInput(outPoint, EmptyScriptSignature, sequence)
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

    subtractFeeFromOutputs(txWithoutFee,
                           feeRate,
                           Vector(changeSPK, remoteChangeSPK))
  }

  def createFundingTransactionSigs(): Future[FundingSignatures] = {
    val fundingTx = createUnsignedFundingTransaction

    val sigFs = fundingUtxos.foldLeft(Vector.empty[Future[PartialSignature]]) {
      case (vec, utxo) =>
        val sigF = BitcoinSignerSingle.signSingle(utxo,
                                                  fundingTx,
                                                  isDummySignature = false)
        vec :+ sigF
    }

    Future.sequence(sigFs).map(FundingSignatures.apply)
  }

  def createFundingTransaction(
      remoteSigs: FundingSignatures): Future[Transaction] = {
    val (localTweak, remoteTweak) = if (isInitiator) {
      (0, fundingUtxos.length)
    } else {
      (remoteFundingInputs.length, 0)
    }

    val fundingPSBT = remoteSigs.sigs.zipWithIndex
      .foldLeft(PSBT.fromUnsignedTx(createUnsignedFundingTransaction)) {
        case (psbt, (sig, index)) =>
          psbt
            .addWitnessUTXOToInput(remoteFundingInputs(index)._2,
                                   index + remoteTweak)
            .addSignature(sig, index + remoteTweak)
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
  def createUnsignedMutualClosePSBT(
      sig: SchnorrDigitalSignature,
      fundingTx: Transaction): PSBT = {
    val (toLocalPayout, toRemotePayout) =
      if (Schnorr.verify(messageWin, sig, oraclePubKey)) {
        (winPayout, remoteWinPayout)
      } else if (Schnorr.verify(messageLose, sig, oraclePubKey)) {
        (losePayout, remoteLosePayout)
      } else {
        throw new IllegalArgumentException(
          s"Signature does not correspond to either possible outcome! $sig")
      }

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
    val input = TransactionInput(
      TransactionOutPoint(fundingTx.txId, UInt32.zero),
      EmptyScriptSignature,
      sequence)
    val utx = BaseTransaction(TransactionConstants.validLockVersion,
                              Vector(input),
                              outputs.filter(_.value >= Policy.dustThreshold),
                              UInt32.zero)

    PSBT
      .fromUnsignedTx(utx)
      .addUTXOToInput(fundingTx, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
  }

  def createMutualCloseTxSig(
      sig: SchnorrDigitalSignature,
      fundingTx: Transaction): Future[PartialSignature] = {
    val unsignedPSBT = createUnsignedMutualClosePSBT(sig, fundingTx)

    unsignedPSBT
      .sign(inputIndex = 0, fundingPrivKey)
      .flatMap(findSigInPSBT(_, fundingPrivKey.publicKey))
  }

  def createMutualCloseTx(
      sig: SchnorrDigitalSignature,
      fundingSig: PartialSignature,
      fundingTx: Transaction): Future[Transaction] = {
    val unsignedPSBT = createUnsignedMutualClosePSBT(sig, fundingTx)

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
      sigPubKey: ECPublicKey,
      remoteSig: PartialSignature,
      payout: CurrencyUnit,
      remotePayout: CurrencyUnit): (Future[Transaction], P2WSHWitnessV0) = {
    val pubKeyBytes = NativeSecp256k1.pubKeyTweakAdd(
      sigPubKey.bytes.toArray,
      cetToLocalPrivKey.bytes.toArray,
      true)
    val pubKey = ECPublicKey.fromBytes(ByteVector(pubKeyBytes))

    val toLocalSPK = P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout),
      timeoutPubKey = cetToLocalRemotePubKey
    )

    val toLocal: TransactionOutput =
      TransactionOutput(payout + toLocalClosingFee,
                        P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout,
                        P2WPKHWitnessSPKV0(cetToRemoteRemotePubKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)

    val fundingTx = createUnsignedFundingTransaction
    val fundingTxid = fundingTx.txId
    val fundingInput = TransactionInput(
      TransactionOutPoint(fundingTxid, UInt32.zero),
      EmptyScriptSignature,
      sequence)

    val psbt = PSBT.fromUnsignedTx(
      BaseTransaction(TransactionConstants.validLockVersion,
                      Vector(fundingInput),
                      outputs.filter(_.value >= Policy.dustThreshold),
                      timeouts.contractMaturity.toUInt32)
    )

    val readyToSignPSBT = psbt
      .addSignature(remoteSig, inputIndex = 0)
      .addUTXOToInput(fundingTx, index = 0)
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
  def createCETRemote(
      sigPubKey: ECPublicKey,
      payout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[
    (Transaction, P2WSHWitnessV0, PartialSignature)] = {
    val pubKey = sigPubKey.add(cetToLocalRemotePubKey)

    val toLocalSPK = P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout),
      timeoutPubKey = cetToLocalPrivKey.publicKey
    )

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout + toLocalClosingFee,
                        P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(payout,
                        P2WPKHWitnessSPKV0(cetToRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)

    val fundingTx = createUnsignedFundingTransaction
    val fundingTxid = fundingTx.txIdBE
    val fundingOutPoint = TransactionOutPoint(fundingTxid, UInt32.zero)
    val fundingInput =
      TransactionInput(fundingOutPoint, EmptyScriptSignature, sequence)

    val unsignedTx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector(fundingInput),
      outputs.filter(_.value >= Policy.dustThreshold),
      timeouts.contractMaturity.toUInt32)

    val sigF = PSBT
      .fromUnsignedTx(unsignedTx)
      .addUTXOToInput(fundingTx, index = 0)
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
      sequence)
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
    val fundingTx = createUnsignedFundingTransaction
    val refundTx = createUnsignedRefundTx

    val sigF = PSBT
      .fromUnsignedTx(refundTx)
      .addUTXOToInput(fundingTx, index = 0)
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
      .addUTXOToInput(createUnsignedFundingTransaction, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), index = 0)
      .sign(inputIndex = 0, fundingPrivKey)

    signedPSBTF.flatMap { signedPSBT =>
      val sigF = findSigInPSBT(signedPSBT, fundingPrivKey.publicKey)

      val txT = signedPSBT.finalizePSBT.flatMap(_.extractTransactionAndValidate)
      val txF = Future.fromTry(txT)
      txF.flatMap(tx => sigF.map(sig => (tx, sig)))
    }
  }

  def createCETWin(
      remoteSig: PartialSignature): (Future[Transaction], P2WSHWitnessV0) = {
    createCET(
      sigPubKey = sigPubKeyWin,
      remoteSig = remoteSig,
      payout = winPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLose(
      remoteSig: PartialSignature): (Future[Transaction], P2WSHWitnessV0) = {
    createCET(
      sigPubKey = sigPubKeyLose,
      remoteSig = remoteSig,
      payout = losePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(): Future[
    (Transaction, P2WSHWitnessV0, PartialSignature)] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      payout = winPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(): Future[
    (Transaction, P2WSHWitnessV0, PartialSignature)] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      payout = losePayout,
      remotePayout = remoteLosePayout
    )
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
      (cetWinRemote, cetWinRemoteWitness, remoteWinSig) <- createCETWinRemote()
      (cetLoseRemote, cetLoseRemoteWitness, remoteLoseSig) <- createCETLoseRemote()
      (_, remoteRefundSig) <- createRefundSig()
      cetSigs = CETSignatures(remoteWinSig, remoteLoseSig, remoteRefundSig)
      _ <- sendSigs(cetSigs)
      (cetSigs, fundingSigs) <- getSigs

      (cetWinLocalF, cetWinLocalWitness) = createCETWin(cetSigs.winSig)
      (cetLoseLocalF, cetLoseLocalWitness) = createCETLose(cetSigs.loseSig)
      cetWinLocal <- cetWinLocalF
      cetLoseLocal <- cetLoseLocalF
      (refundTx, _) <- createRefundTx(cetSigs.refundSig)
      fundingTx <- createFundingTransaction(fundingSigs)
    } yield {
      SetupDLC(
        fundingTx,
        cetWinLocal,
        cetWinLocalWitness,
        cetLoseLocal,
        cetLoseLocalWitness,
        cetWinRemote.txIdBE,
        cetWinRemoteWitness,
        cetLoseRemote.txIdBE,
        cetLoseRemoteWitness,
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
      case CETSignatures(winSig, loseSig, refundSig) =>
        // Construct all CETs
        val (cetWinLocalF, cetWinLocalWitness) = createCETWin(winSig)
        val (cetLoseLocalF, cetLoseLocalWitness) = createCETLose(loseSig)
        val cetWinRemoteF = createCETWinRemote()
        val cetLoseRemoteF = createCETLoseRemote()
        val refundTxF = createRefundTx(refundSig)

        for {
          cetWinLocal <- cetWinLocalF
          cetLoseLocal <- cetLoseLocalF
          (cetWinRemote, cetWinRemoteWitness, remoteWinSig) <- cetWinRemoteF
          (cetLoseRemote, cetLoseRemoteWitness, remoteLoseSig) <- cetLoseRemoteF
          (refundTx, remoteRefundSig) <- refundTxF
          cetSigs = CETSignatures(remoteWinSig, remoteLoseSig, remoteRefundSig)
          localFundingSigs <- createFundingTransactionSigs()
          _ <- sendSigs(cetSigs, localFundingSigs)
          fundingTx <- getFundingTx
        } yield {
          SetupDLC(
            fundingTx,
            cetWinLocal,
            cetWinLocalWitness,
            cetLoseLocal,
            cetLoseLocalWitness,
            cetWinRemote.txIdBE,
            cetWinRemoteWitness,
            cetLoseRemote.txIdBE,
            cetLoseRemoteWitness,
            refundTx
          )
        }
    }
  }

  def constructClosingTx(
      privKey: ECPrivateKey,
      spendingInfo: BitcoinUTXOSpendingInfoFull,
      isWin: Boolean,
      spendsToLocal: Boolean): Future[Option[Transaction]] = {
    // If spendsToLocal, use payout as value, otherwise subtract fee
    val spendingTxOptF = if (spendsToLocal) {
      val payoutValue = if (isWin) {
        winPayout
      } else {
        losePayout
      }

      if (payoutValue < Policy.dustThreshold) {
        Future.successful(None)
      } else {

        val txBuilder = BitcoinTxBuilder(
          destinations = Vector(
            TransactionOutput(payoutValue,
                              P2WPKHWitnessSPKV0(privKey.publicKey))),
          utxos = Vector(spendingInfo),
          feeRate = feeRate,
          changeSPK = emptyChangeSPK,
          network = network
        )

        txBuilder.flatMap(_.sign).map(Some(_))
      }
    } else {
      val txBuilder = BitcoinTxBuilder(
        destinations = Vector(
          TransactionOutput(spendingInfo.output.value,
                            P2WPKHWitnessSPKV0(privKey.publicKey))),
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
      fundingSig <- createMutualCloseTxSig(sig, fundingTx)
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
      mutualCloseTx <- createMutualCloseTx(sig, fundingSig, fundingTx)
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
    val SetupDLC(fundingTx,
                 cetWinLocal,
                 cetWinLocalWitness,
                 cetLoseLocal,
                 cetLoseLocalWitness,
                 _,
                 _,
                 _,
                 _,
                 _) = dlcSetup

    oracleSigF.flatMap { oracleSig =>
      val sigForWin = Schnorr.verify(messageWin, oracleSig, oraclePubKey)
      val sigForLose = Schnorr.verify(messageLose, oracleSig, oraclePubKey)

      // Pick the CET to use and payout by checking which message was signed
      val (cet, cetScriptWitness) =
        if (sigForWin) {
          (cetWinLocal, cetWinLocalWitness)
        } else if (sigForLose) {
          (cetLoseLocal, cetLoseLocalWitness)
        } else {
          throw new IllegalArgumentException(
            s"Signature does not correspond to either possible outcome! $oracleSig")
        }

      val output = cet.outputs.head

      val privKeyBytes = NativeSecp256k1.privKeyTweakAdd(
        cetToLocalPrivKey.bytes.toArray,
        oracleSig.s.toArray
      )
      val privKey = ECPrivateKey.fromBytes(ByteVector(privKeyBytes))

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
        val localSpendingTxF = constructClosingTx(finalPrivKey,
                                                  cetSpendingInfo,
                                                  isWin = sigForWin,
                                                  spendsToLocal = true)

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

  /** Constructs the closing transaction on the to_remote output of a counter-party's unilateral CET broadcast
    * @see [[https://github.com/discreetlogcontracts/dlcspecs/blob/master/Transactions.md#closing-transaction-unilateral]]
    */
  def executeRemoteUnilateralDLC(
      dlcSetup: SetupDLC,
      publishedCET: Transaction): Future[UnilateralDLCOutcome] = {
    val output = publishedCET.outputs.last

    val isWin = publishedCET.txIdBE != dlcSetup.cetWinRemoteTxid

    val spendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(publishedCET.txIdBE, UInt32.one),
      amount = output.value,
      scriptPubKey = P2WPKHWitnessSPKV0(cetToRemotePrivKey.publicKey),
      signer = cetToRemotePrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetToRemotePrivKey.publicKey)
    )

    if (isToLocalOutput(output)) {
      Future.successful(
        UnilateralDLCOutcomeWithDustClosing(fundingTx = dlcSetup.fundingTx,
                                            cet = publishedCET)
      )
    } else {
      val txF =
        constructClosingTx(privKey = finalPrivKey,
                           spendingInfo = spendingInfo,
                           isWin = isWin,
                           spendsToLocal = false)

      txF.map {
        case Some(tx) =>
          UnilateralDLCOutcomeWithClosing(
            fundingTx = dlcSetup.fundingTx,
            cet = publishedCET,
            closingTx = tx,
            cetSpendingInfo = spendingInfo
          )
        case None =>
          UnilateralDLCOutcomeWithDustClosing(fundingTx = dlcSetup.fundingTx,
                                              cet = publishedCET)
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

    val (cetScriptWitness, isWin) =
      if (timedOutCET.txIdBE == dlcSetup.cetWinRemoteTxid) {
        (dlcSetup.cetWinRemoteWitness, false)
      } else {
        (dlcSetup.cetLoseRemoteWitness, true)
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
        constructClosingTx(privKey = finalPrivKey,
                           spendingInfo = justiceSpendingInfo,
                           isWin = isWin,
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
    val SetupDLC(fundingTx, _, _, _, _, _, _, _, _, refundTx) =
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

    val localSpendingTxF = constructClosingTx(finalPrivKey,
                                              localRefundSpendingInfo,
                                              isWin = false,
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

object BinaryOutcomeDLCClient {

  def apply(
      outcomeWin: String,
      outcomeLose: String,
      oraclePubKey: ECPublicKey,
      preCommittedR: ECPublicKey,
      isInitiator: Boolean,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      remotePubKeys: DLCPublicKeys,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
      remoteFundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
      winPayout: CurrencyUnit,
      losePayout: CurrencyUnit,
      timeouts: DLCTimeouts,
      feeRate: FeeUnit,
      changeSPK: WitnessScriptPubKeyV0,
      remoteChangeSPK: WitnessScriptPubKeyV0,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): BinaryOutcomeDLCClient = {
    val hashWin = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
    val hashLose = CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip

    BinaryOutcomeDLCClient(
      outcomeWin = hashWin,
      outcomeLose = hashLose,
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
      winPayout = winPayout,
      losePayout = losePayout,
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
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): BinaryOutcomeDLCClient = {
    BinaryOutcomeDLCClient(
      outcomeWin = offer.contractInfo.keys.head,
      outcomeLose = offer.contractInfo.keys.last,
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
      winPayout = offer.contractInfo.head._2,
      losePayout = offer.contractInfo.last._2,
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
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): BinaryOutcomeDLCClient = {
    require(
      DLCPublicKeys
        .fromExtPrivKeyAndIndex(extPrivKey, nextAddressIndex, network) == offer.pubKeys,
      "ExtPrivateKey must match the one in your Offer message")
    require(
      fundingUtxos.zip(offer.fundingInputs).forall {
        case (info, (outPoint, output)) =>
          info.output == output && info.outPoint == outPoint
      },
      "Funding UTXOs must match those in your Offer message"
    )

    BinaryOutcomeDLCClient(
      outcomeWin = offer.contractInfo.keys.head,
      outcomeLose = offer.contractInfo.keys.last,
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
      winPayout = offer.contractInfo.head._2,
      losePayout = offer.contractInfo.last._2,
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

case class FundingSignatures(sigs: Vector[PartialSignature])

case class CETSignatures(
    winSig: PartialSignature,
    loseSig: PartialSignature,
    refundSig: PartialSignature)
