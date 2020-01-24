package org.bitcoins.dlc

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  ExtPublicKey,
  Schnorr,
  SchnorrDigitalSignature
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.hd.{BIP32Node, BIP32Path}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.BlockStampWithFuture
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
  TransactionOutput
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
  P2WSHV0SpendingInfoFull,
  P2WSHV0SpendingInfoSingle
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
  * @param extPrivKey This client's extended private key for this event
  * @param remoteExtPubKey Remote's extended public key for this event
  * @param input This client's total collateral contribution
  * @param remoteInput Remote's total collateral contribution
  * @param fundingUtxos This client's funding BitcoinUTXOSpendingInfo collection
  * @param remoteFundingInputs Remote's funding inputs and their values
  * @param winPayout This client's payout in the Win case
  * @param losePayout This client's payout in the Lose case
  * @param timeouts The timeouts for this DLC
  * @param feeRate The predicted fee rate used for all transactions
  * @param changeSPK This client's change ScriptPubKey used in the funding tx
  * @param remoteChangeSPK Remote's change ScriptPubKey used in the funding tx
  */
case class BinaryOutcomeDLCClient(
    outcomeWin: String,
    outcomeLose: String,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey,
    extPrivKey: ExtPrivateKey,
    remoteExtPubKey: ExtPublicKey,
    input: CurrencyUnit,
    remoteInput: CurrencyUnit,
    fundingUtxos: Vector[BitcoinUTXOSpendingInfoSingle],
    remoteFundingInputs: Vector[(TransactionOutPoint, CurrencyUnit)],
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
  val messageWin: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip.bytes

  /** Hash signed by oracle in Lose case */
  val messageLose: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip.bytes

  /** sig*G in the Win case */
  val sigPubKeyWin: ECPublicKey =
    Schnorr.computePubKey(messageWin, preCommittedR, oraclePubKey)

  /** sig*G in the Lose case */
  val sigPubKeyLose: ECPublicKey =
    Schnorr.computePubKey(messageLose, preCommittedR, oraclePubKey)

  val fundingPrivKey: ECPrivateKey =
    extPrivKey.deriveChildPrivKey(UInt32(0)).key

  val fundingRemotePubKey: ECPublicKey =
    remoteExtPubKey.deriveChildPubKey(UInt32(0)).get.key

  val finalPrivKey: ECPrivateKey =
    extPrivKey.deriveChildPrivKey(UInt32(2)).key

  val finalRemotePubKey: ECPublicKey =
    remoteExtPubKey.deriveChildPubKey(UInt32(2)).get.key

  val winIsFirst: Boolean = outcomeWin.compareTo(outcomeLose) > 0

  /** The derivation index for the win and lose cases respectively.
    * We assign this index based on lexicographical order to keep things deterministic.
    */
  private val (winIndex, loseIndex) =
    if (winIsFirst) {
      (1, 2)
    } else {
      (2, 1)
    }

  def cetExtPrivKey(rootKey: ExtPrivateKey, eventIndex: Int): ExtPrivateKey = {
    rootKey
      .deriveChildPrivKey(
        BIP32Path(BIP32Node(1, hardened = false),
                  BIP32Node(eventIndex, hardened = false)))
  }

  val cetRefundPrivKey: ECPrivateKey =
    cetExtPrivKey(extPrivKey, eventIndex = 0).key

  val cetWinPrivKey: ExtPrivateKey =
    cetExtPrivKey(extPrivKey, winIndex)

  val cetLosePrivKey: ExtPrivateKey =
    cetExtPrivKey(extPrivKey, loseIndex)

  def cetExtPubKey(rootKey: ExtPublicKey, eventIndex: Int): ExtPublicKey = {
    rootKey
      .deriveChildPubKey(
        BIP32Path(BIP32Node(1, hardened = false),
                  BIP32Node(eventIndex, hardened = false)))
      .get
  }

  val cetRemoteRefundPubKey: ECPublicKey =
    cetExtPubKey(remoteExtPubKey, eventIndex = 0).key

  val cetRemoteWinPubKey: ExtPublicKey =
    cetExtPubKey(remoteExtPubKey, winIndex)

  val cetRemoteLosePubKey: ExtPublicKey =
    cetExtPubKey(remoteExtPubKey, loseIndex)

  /** Total collateral amount */
  private val totalInput = input + remoteInput

  private val totalFunding =
    fundingUtxos.foldLeft(0L)(_ + _.amount.satoshis.toLong)
  private val remoteTotalFunding =
    remoteFundingInputs.foldLeft(0L)(_ + _._2.satoshis.toLong)

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
    MultiSignatureScriptPubKey(2, Vector(fundingPubKey, fundingRemotePubKey))
  }

  /** Experimental approx. vbytes for a CET */
  private val approxCETVBytes = 190

  /** Experimental approx. vbytes for a closing tx spending ToLocalOutput */
  private val approxToLocalClosingVBytes = 122

  private val cetFee: CurrencyUnit = Satoshis(approxCETVBytes * feeRate.toLong)
  private val toLocalClosingFee: CurrencyUnit = Satoshis(
    approxToLocalClosingVBytes * feeRate.toLong)

  private val isRBFEnabled = Policy.isRBFEnabled
  private val sequence =
    if (isRBFEnabled) UInt32.zero else TransactionConstants.sequence

  private lazy val createUnsignedFundingTransaction: Transaction = {
    /* We need to commit to the CET's and local closing tx's fee during the construction of
     * the funding transaction so that the CET outputs have the expected payouts.
     *
     * Once computed, we add the estimated amount to the fundingOutput so it can be used for fees later.
     */
    val halfCetFee = Satoshis((cetFee + toLocalClosingFee).satoshis.toLong / 2)

    val output: TransactionOutput =
      TransactionOutput(totalInput + halfCetFee + halfCetFee,
                        P2WSHWitnessSPKV0(fundingSPK))
    val change =
      TransactionOutput(Satoshis(totalFunding) - input - halfCetFee, changeSPK)
    val remoteChange =
      TransactionOutput(Satoshis(remoteTotalFunding) - remoteInput - halfCetFee,
                        remoteChangeSPK)

    val outputs: Vector[TransactionOutput] =
      Vector(output, change, remoteChange)

    val localInputs =
      TxBuilder.calcSequenceForInputs(fundingUtxos, isRBFEnabled)
    val remoteInputs = remoteFundingInputs.map(outpoint =>
      TransactionInput(outpoint._1, EmptyScriptSignature, sequence))
    val inputs = localInputs ++ remoteInputs

    val txWithoutFee = BaseTransaction(version =
                                         TransactionConstants.validLockVersion,
                                       inputs = inputs,
                                       outputs = outputs,
                                       lockTime = UInt32.zero)

    subtractFeeFromOutputs(txWithoutFee,
                           feeRate,
                           Vector(changeSPK, remoteChangeSPK))
  }

  def createFundingTransactionSigs(): Future[Vector[PartialSignature]] = {
    val fundingTx = createUnsignedFundingTransaction

    val sigFs = fundingUtxos.foldLeft(Vector.empty[Future[PartialSignature]]) {
      case (vec, utxo) =>
        val sigF = BitcoinSignerSingle.signSingle(utxo,
                                                  fundingTx,
                                                  isDummySignature = false)
        vec :+ sigF
    }

    Future.sequence(sigFs)
  }

  def createFundingTransaction(
      remoteSigs: Vector[PartialSignature]): Future[Transaction] = {
    val fundingPSBT = remoteSigs.zipWithIndex.foldLeft(
      PSBT.fromUnsignedTx(createUnsignedFundingTransaction)) {
      case (psbt, (sig, index)) =>
        psbt.addSignature(sig, index + fundingUtxos.length)
    }

    val signedFundingPSBTF =
      fundingUtxos.zipWithIndex.foldLeft(Future.successful(fundingPSBT)) {
        case (psbtF, (utxo, index)) =>
          psbtF.flatMap(_.sign(index, utxo.signers.head))
      }

    signedFundingPSBTF.flatMap { signedFundingPSBT =>
      Future.fromTry(signedFundingPSBT.extractTransactionAndValidate)
    }
  }

  /** Constructs CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCET(
      sigPubKey: ECPublicKey,
      remoteSig: PartialSignature,
      payout: CurrencyUnit,
      remotePayout: CurrencyUnit): (Future[Transaction], P2WSHWitnessV0) = {
    val (cetPrivKey, cetRemotePubKey) = if (sigPubKey == sigPubKeyWin) {
      (cetWinPrivKey, cetRemoteWinPubKey)
    } else {
      (cetLosePrivKey, cetRemoteLosePubKey)
    }

    val toLocalPrivKey =
      cetPrivKey.deriveChildPrivKey(UInt32.zero).key
    val remoteToLocalPubKey =
      cetRemotePubKey.deriveChildPubKey(UInt32.one).get.key
    val toRemotePubKey = cetRemotePubKey.deriveChildPubKey(UInt32(2)).get.key

    val pubKeyBytes = NativeSecp256k1.pubKeyTweakAdd(
      sigPubKey.bytes.toArray,
      toLocalPrivKey.bytes.toArray,
      true)
    val pubKey = ECPublicKey.fromBytes(ByteVector(pubKeyBytes))

    val toLocalSPK = P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout),
      timeoutPubKey = remoteToLocalPubKey
    )

    val toLocal: TransactionOutput =
      TransactionOutput(payout + toLocalClosingFee,
                        P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout, P2WPKHWitnessSPKV0(toRemotePubKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)

    val fundingTxid = createUnsignedFundingTransaction.txId
    val fundingInput = TransactionInput(
      TransactionOutPoint(fundingTxid, UInt32.zero),
      EmptyScriptSignature,
      sequence)

    val psbt = PSBT.fromUnsignedTx(
      BaseTransaction(TransactionConstants.validLockVersion,
                      Vector(fundingInput),
                      outputs,
                      UInt32.zero)
    )

    val signedPSBTF = psbt
      .addSignature(remoteSig, inputIndex = 0)
      .sign(inputIndex = 0, fundingPrivKey)

    val signedCETF = signedPSBTF.flatMap { signedPSBT =>
      Future.fromTry(signedPSBT.extractTransactionAndValidate)
    }

    (signedCETF, P2WSHWitnessV0(toLocalSPK))
  }

  /** Constructs Remote's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETRemote(
      sigPubKey: ECPublicKey,
      payout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[
    (Transaction, PartialSignature, P2WSHWitnessV0)] = {
    val (cetLocalPrivKey, cetRemotePubKey) = if (sigPubKey == sigPubKeyWin) {
      (cetWinPrivKey, cetRemoteWinPubKey)
    } else {
      (cetLosePrivKey, cetRemoteLosePubKey)
    }

    val remoteToLocalPubKey =
      cetRemotePubKey.deriveChildPubKey(UInt32.zero).get.key
    val localToLocalPrivKey = cetLocalPrivKey.deriveChildPrivKey(UInt32.one).key
    val toRemotePrivKey = cetLocalPrivKey.deriveChildPrivKey(UInt32(2)).key

    val pubKey = sigPubKey.add(remoteToLocalPubKey)

    val toLocalSPK = P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout),
      timeoutPubKey = localToLocalPrivKey.publicKey
    )

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout + toLocalClosingFee,
                        P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(payout, P2WPKHWitnessSPKV0(toRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)

    val fundingTx = createUnsignedFundingTransaction
    val fundingTxid = fundingTx.txIdBE
    val fundingOutput = fundingTx.outputs.head
    val fundingOutPoint = TransactionOutPoint(fundingTxid, UInt32.zero)
    val fundingInput =
      TransactionInput(fundingOutPoint, EmptyScriptSignature, sequence)

    val unsignedTx = BaseTransaction(TransactionConstants.validLockVersion,
                                     Vector(fundingInput),
                                     outputs,
                                     UInt32.zero)

    val sigF = BitcoinSignerSingle.signSingle(
      spendingInfo = P2WSHV0SpendingInfoSingle(
        outPoint = fundingOutPoint,
        amount = fundingOutput.value,
        scriptPubKey = P2WSHWitnessSPKV0(fundingSPK),
        signer = fundingPrivKey,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WSHWitnessV0(fundingSPK),
        conditionalPath = ConditionalPath.NoConditionsLeft
      ),
      unsignedTx = unsignedTx,
      isDummySignature = false
    )

    sigF.map((unsignedTx, _, P2WSHWitnessV0(toLocalSPK)))
  }

  /** Constructs the (time-locked) refund transaction for when the oracle disappears
    * or signs an unknown message.
    * Note that both parties have the same refund transaction.
    */
  def createRefundTx(
      remoteSig: PartialSignature): Future[(Transaction, PartialSignature)] = {
    val fundingTx = createUnsignedFundingTransaction
    val fundingTxid = fundingTx.txId
    val fundingInput = TransactionInput(
      TransactionOutPoint(fundingTxid, UInt32.zero),
      EmptyScriptSignature,
      sequence)
    val fundingOutput = fundingTx.outputs.head

    val toLocalValueNotSat =
      (fundingOutput.value * input).satoshis.toLong / totalInput.satoshis.toLong
    val toLocalValue = Satoshis(toLocalValueNotSat)
    val toRemoteValue = fundingOutput.value - toLocalValue

    val toLocal = TransactionOutput(
      toLocalValue,
      P2WPKHWitnessSPKV0(cetRefundPrivKey.publicKey))
    val toRemote = TransactionOutput(toRemoteValue,
                                     P2WPKHWitnessSPKV0(cetRemoteRefundPubKey))

    val outputs = Vector(toLocal, toRemote)

    val psbt = PSBT.fromUnsignedTx(
      BaseTransaction(TransactionConstants.validLockVersion,
                      Vector(fundingInput),
                      outputs,
                      UInt32.zero)
    )

    val signedPSBTF = psbt
      .addSignature(remoteSig, inputIndex = 0)
      .sign(inputIndex = 0, fundingPrivKey)

    signedPSBTF.flatMap { signedPSBT =>
      val sig = signedPSBT.inputMaps.head.partialSignatures
        .filter(_.pubKey == fundingPubKey)
        .head

      val txF = Future.fromTry(signedPSBT.extractTransactionAndValidate)
      txF.map((_, sig))
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
    (Transaction, PartialSignature, P2WSHWitnessV0)] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      payout = winPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(): Future[
    (Transaction, PartialSignature, P2WSHWitnessV0)] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      payout = losePayout,
      remotePayout = remoteLosePayout
    )
  }

  def setupDLC(
      getSigs: Future[(PartialSignature, PartialSignature, PartialSignature)],
      sendSigs: (
          PartialSignature,
          PartialSignature,
          PartialSignature,
          Vector[PartialSignature]) => Future[Unit],
      getFundingSigs: Future[Vector[PartialSignature]]): Future[SetupDLC] = {
    getSigs.flatMap {
      case (refundSig, firstSig, secondSig) =>
        val (winSig, loseSig) = if (winIsFirst) {
          (firstSig, secondSig)
        } else {
          (secondSig, firstSig)
        }

        // Construct all CETs
        val (cetWinLocalF, cetWinLocalWitness) = createCETWin(winSig)
        val (cetLoseLocalF, cetLoseLocalWitness) = createCETLose(loseSig)
        val cetWinRemoteF = createCETWinRemote()
        val cetLoseRemoteF = createCETLoseRemote()
        val refundTxF = createRefundTx(refundSig)

        cetWinLocalF.foreach(cet => logger.info(s"CET Win Local: ${cet.hex}\n"))
        cetLoseLocalF.foreach(cet =>
          logger.info(s"CET Lose Local: ${cet.hex}\n"))
        cetWinRemoteF.foreach(cet =>
          logger.info(s"CET Win Remote: ${cet._1.hex}\n"))
        cetLoseRemoteF.foreach(cet =>
          logger.info(s"CET Lose Remote: ${cet._1.hex}\n"))
        refundTxF.foreach(refundTx =>
          logger.info(s"Refund Tx: ${refundTx._1.hex}\n"))

        for {
          cetWinLocal <- cetWinLocalF
          cetLoseLocal <- cetLoseLocalF
          (cetWinRemote, remoteWinSig, cetWinRemoteWitness) <- cetWinRemoteF
          (cetLoseRemote, remoteLoseSig, cetLoseRemoteWitness) <- cetLoseRemoteF
          (refundTx, remoteRefundSig) <- refundTxF
          localFundingSigs <- createFundingTransactionSigs()
          _ <- {
            if (winIsFirst) {
              sendSigs(remoteRefundSig,
                       remoteWinSig,
                       remoteLoseSig,
                       localFundingSigs)
            } else {
              sendSigs(remoteRefundSig,
                       remoteLoseSig,
                       remoteWinSig,
                       localFundingSigs)
            }
          }
          fundingSigs <- getFundingSigs
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
  }

  def constructClosingTx(
      privKey: ECPrivateKey,
      spendingInfo: BitcoinUTXOSpendingInfoFull,
      isWin: Boolean,
      spendsToLocal: Boolean): Future[Transaction] = {
    // If spendsToLocal, use payout as value, otherwise subtract fee
    val spendingTxF = if (spendsToLocal) {
      val payoutValue = if (isWin) {
        winPayout
      } else {
        losePayout
      }

      val txBuilder = BitcoinTxBuilder(
        destinations = Vector(
          TransactionOutput(payoutValue,
                            P2WPKHWitnessSPKV0(privKey.publicKey))),
        utxos = Vector(spendingInfo),
        feeRate = feeRate,
        changeSPK = emptyChangeSPK,
        network = network
      )

      txBuilder.flatMap(_.sign)
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

      txBuilder.flatMap(subtractFeeAndSign)
    }

    spendingTxF.foreach(tx => logger.info(s"Closing Tx: ${tx.hex}"))

    spendingTxF
  }

  /** Constructs and executes on the unilateral spending branch of a DLC
    *
    * @return Each transaction published and its spending info
    */
  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSigF: Future[SchnorrDigitalSignature]): Future[DLCOutcome] = {
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
      val (cet, extCetPrivKey, cetScriptWitness) =
        if (sigForWin) {
          (cetWinLocal, cetWinPrivKey, cetWinLocalWitness)
        } else if (sigForLose) {
          (cetLoseLocal, cetLosePrivKey, cetLoseLocalWitness)
        } else {
          throw new IllegalStateException(
            "Signature does not correspond to either possible outcome!")
        }

      val cetPrivKey = extCetPrivKey.deriveChildPrivKey(UInt32.zero).key

      // The prefix other refers to remote if local == true and local otherwise
      val output = cet.outputs.head

      val privKeyBytes = NativeSecp256k1.privKeyTweakAdd(
        cetPrivKey.bytes.toArray,
        oracleSig.s.toArray
      )
      val privKey = ECPrivateKey.fromBytes(ByteVector(privKeyBytes))

      // Spend the true case on the correct CET
      val cetSpendingInfo = P2WSHV0SpendingInfoFull(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.zero),
        amount = output.value,
        scriptPubKey = output.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
        signersWithPossibleExtra = Vector(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = cetScriptWitness,
        conditionalPath = ConditionalPath.nonNestedTrue
      )

      val localSpendingTxF = constructClosingTx(finalPrivKey,
                                                cetSpendingInfo,
                                                isWin = sigForWin,
                                                spendsToLocal = true)

      localSpendingTxF.map { localSpendingTx =>
        DLCOutcome(
          fundingTx = fundingTx,
          cet = cet,
          closingTx = localSpendingTx,
          cetSpendingInfo = cetSpendingInfo
        )
      }
    }
  }

  def executeRemoteUnilateralDLC(
      dlcSetup: SetupDLCWithSelf,
      publishedCET: Transaction): Future[DLCOutcome] = {
    val output = publishedCET.outputs.last

    val (extCetPrivKey, isWin) =
      if (publishedCET == dlcSetup.cetWinRemote) {
        (cetWinPrivKey, false)
      } else {
        (cetLosePrivKey, true)
      }

    val cetPrivKeyToRemote = extCetPrivKey.deriveChildPrivKey(UInt32(2)).key

    val spendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(publishedCET.txIdBE, UInt32.one),
      amount = output.value,
      scriptPubKey = output.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = cetPrivKeyToRemote,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetPrivKeyToRemote.publicKey)
    )

    val txF =
      constructClosingTx(privKey = finalPrivKey,
                         spendingInfo = spendingInfo,
                         isWin = isWin,
                         spendsToLocal = false)

    txF.map { tx =>
      DLCOutcome(
        fundingTx = dlcSetup.fundingTx,
        cet = publishedCET,
        closingTx = tx,
        cetSpendingInfo = spendingInfo
      )
    }
  }

  /** Constructs and executes on the justice spending branch of a DLC
    * where a published CET has timed out.
    *
    * @return Each transaction published and its spending info
    */
  def executeJusticeDLC(
      dlcSetup: SetupDLCWithSelf,
      timedOutCET: Transaction): Future[DLCOutcome] = {
    val justiceOutput = timedOutCET.outputs.head

    val (extCetPrivKey, cetScriptWitness, isWin) =
      if (timedOutCET == dlcSetup.cetWinRemote) {
        (cetWinPrivKey, dlcSetup.cetWinRemoteWitness, false)
      } else {
        (cetLosePrivKey, dlcSetup.cetLoseRemoteWitness, true)
      }

    val cetPrivKeyJustice = extCetPrivKey.deriveChildPrivKey(UInt32.one).key

    val justiceSpendingInfo = P2WSHV0SpendingInfoFull(
      outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.zero),
      amount = justiceOutput.value,
      scriptPubKey = justiceOutput.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
      signersWithPossibleExtra = Vector(cetPrivKeyJustice),
      hashType = HashType.sigHashAll,
      scriptWitness = cetScriptWitness,
      conditionalPath = ConditionalPath.nonNestedFalse
    )

    val justiceSpendingTxF =
      constructClosingTx(privKey = finalPrivKey,
                         spendingInfo = justiceSpendingInfo,
                         isWin = isWin,
                         spendsToLocal = false)

    justiceSpendingTxF.map { justiceSpendingTx =>
      DLCOutcome(
        fundingTx = dlcSetup.fundingTx,
        cet = timedOutCET,
        closingTx = justiceSpendingTx,
        cetSpendingInfo = justiceSpendingInfo
      )
    }
  }

  /** Constructs and executes on the refund spending branch of a DLC
    *
    * @return Each transaction published and its spending info
    */
  def executeRefundDLC(dlcSetup: SetupDLC): Future[DLCOutcome] = {
    val SetupDLC(fundingTx, _, _, _, _, _, _, _, _, refundTx) =
      dlcSetup

    val localOutput = refundTx.outputs.head

    val localRefundSpendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, UInt32.zero),
      amount = localOutput.value,
      scriptPubKey = localOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = cetRefundPrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetRefundPrivKey.publicKey)
    )

    val localSpendingTxF = constructClosingTx(finalPrivKey,
                                              localRefundSpendingInfo,
                                              isWin = false,
                                              spendsToLocal = false)

    localSpendingTxF.map { localSpendingTx =>
      DLCOutcome(
        fundingTx = fundingTx,
        cet = refundTx,
        closingTx = localSpendingTx,
        cetSpendingInfo = localRefundSpendingInfo
      )
    }
  }
}

object BinaryOutcomeDLCClient {

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
    // TODO: feeRate probably needs to be fleshed out as tx doesn't have dummy sigs in
    val allOuputsWithNew = subtractFees(tx, feeRate, spks)

    BaseTransaction(tx.version, tx.inputs, allOuputsWithNew, tx.lockTime)
  }
}

case class SetupDLC(
    fundingTx: Transaction,
    cetWin: Transaction,
    cetWinWitness: P2WSHWitnessV0,
    cetLose: Transaction,
    cetLoseWitness: P2WSHWitnessV0,
    cetWinRemoteTxid: DoubleSha256DigestBE,
    cetWinRemoteWitness: P2WSHWitnessV0,
    cetLoseRemoteTxid: DoubleSha256DigestBE,
    cetLoseRemoteWitness: P2WSHWitnessV0,
    refundTx: Transaction)

// TODO: Add info to validate fundingTx
/** Contains all DLC transactions and the BitcoinUTXOSpendingInfos they use. */
case class DLCOutcome(
    fundingTx: Transaction,
    cet: Transaction,
    closingTx: Transaction,
    cetSpendingInfo: BitcoinUTXOSpendingInfoFull
)

/** @param penaltyTimeout The CSV timeout in blocks used in all CETs
  * @param contractMaturity The CLTV in milliseconds when a signature is expected
  * @param contractTimeout The CLTV timeout in milliseconds after which the refund tx is valid
  */
case class DLCTimeouts(
    penaltyTimeout: Int,
    contractMaturity: BlockStampWithFuture,
    contractTimeout: BlockStampWithFuture
)
