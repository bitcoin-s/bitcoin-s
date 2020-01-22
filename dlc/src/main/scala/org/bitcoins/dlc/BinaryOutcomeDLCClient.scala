package org.bitcoins.dlc

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
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
import org.bitcoins.core.protocol.BlockStampWithFuture
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
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
  Transaction,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
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
    fundingUtxos: Vector[BitcoinUTXOSpendingInfoFull],
    remoteFundingInputs: Vector[(TransactionInput, CurrencyUnit)],
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

  /** The derivation index for the win and lose cases respectively.
    * We assign this index based on lexicographical order to keep things deterministic.
    */
  private val (winIndex, loseIndex) =
    if (outcomeWin.compareTo(outcomeLose) > 0) {
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

  private lazy val (createUnsignedFundingTransaction, fundingTxBuilderF): (
      Future[Transaction],
      Future[BitcoinTxBuilder]) = {

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
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs,
                       utxos = Vector(???),
                       feeRate,
                       emptyChangeSPK,
                       network)

    val txAndBuilderF = txBuilderF.flatMap { txBuilder =>
      for {
        newBuilder <- subtractFeeFromOutputs(txBuilder,
                                             Vector(changeSPK, remoteChangeSPK))
        unsignedTx <- newBuilder.unsignedTx
      } yield (unsignedTx, newBuilder)
    }

    (txAndBuilderF.map(_._1), txAndBuilderF.map(_._2))
  }

  def createFundingTransaction: Future[Transaction] = {
    fundingTxBuilderF.flatMap(_.sign(noEmptyOutputs))
  }

  /** Constructs CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCET(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: P2WSHV0SpendingInfoFull,
      payout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      invariant: (Seq[BitcoinUTXOSpendingInfoFull], Transaction) => Boolean =
        noEmptyOutputs): Future[(Transaction, P2WSHWitnessV0)] = {
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

    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       emptyChangeSPK,
                       network,
                       timeouts.contractMaturity.toUInt32)

    txBuilderF
      .flatMap(_.sign(invariant))
      .map((_, P2WSHWitnessV0(toLocalSPK)))
  }

  /** Constructs Remote's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: P2WSHV0SpendingInfoFull,
      payout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      invariant: (Seq[BitcoinUTXOSpendingInfoFull], Transaction) => Boolean =
        noEmptyOutputs): Future[(Transaction, P2WSHWitnessV0)] = {
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

    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       emptyChangeSPK,
                       network,
                       timeouts.contractMaturity.toUInt32)

    txBuilderF
      .flatMap(_.sign(invariant))
      .map((_, P2WSHWitnessV0(toLocalSPK)))
  }

  /** Constructs the (time-locked) refund transaction for when the oracle disappears
    * or signs an unknown message.
    * Note that both parties have the same refund transaction.
    */
  def createRefundTx(
      fundingSpendingInfo: P2WSHV0SpendingInfoFull): Future[Transaction] = {
    val toLocalValueNotSat =
      (fundingSpendingInfo.amount * input).satoshis.toLong / totalInput.satoshis.toLong
    val toLocalValue = Satoshis(toLocalValueNotSat)
    val toRemoteValue = fundingSpendingInfo.amount - toLocalValue

    val toLocal = TransactionOutput(
      toLocalValue,
      P2WPKHWitnessSPKV0(cetRefundPrivKey.publicKey))
    val toRemote = TransactionOutput(toRemoteValue,
                                     P2WPKHWitnessSPKV0(cetRemoteRefundPubKey))

    val outputs = Vector(toLocal, toRemote)
    val txBuilderF = BitcoinTxBuilder(outputs,
                                      Vector(fundingSpendingInfo),
                                      feeRate,
                                      emptyChangeSPK,
                                      network,
                                      timeouts.contractTimeout.toUInt32)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  def createCETWin(fundingSpendingInfo: P2WSHV0SpendingInfoFull): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCET(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      payout = winPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLose(fundingSpendingInfo: P2WSHV0SpendingInfoFull): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCET(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      payout = losePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(fundingSpendingInfo: P2WSHV0SpendingInfoFull): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      payout = winPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(fundingSpendingInfo: P2WSHV0SpendingInfoFull): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      payout = losePayout,
      remotePayout = remoteLosePayout
    )
  }

  def setupDLC(): Future[SetupDLC] = {
    // Construct Funding Transaction
    createUnsignedFundingTransaction.flatMap { unsignedFundingTx =>
      val fundingTxId = unsignedFundingTx.txIdBE
      val output = unsignedFundingTx.outputs.head

      val fundingSpendingInfo = P2WSHV0SpendingInfoFull(
        outPoint = TransactionOutPoint(fundingTxId, UInt32.zero),
        amount = output.value,
        scriptPubKey = output.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
        signers = Vector(fundingPrivKey, ???),
        hashType = HashType.sigHashAll,
        scriptWitness = P2WSHWitnessV0(fundingSPK),
        conditionalPath = ConditionalPath.NoConditionsLeft
      )

      // Construct all CETs
      val cetWinLocalF = createCETWin(fundingSpendingInfo)
      val cetLoseLocalF = createCETLose(fundingSpendingInfo)
      val cetWinRemoteF = createCETWinRemote(fundingSpendingInfo)
      val cetLoseRemoteF = createCETLoseRemote(fundingSpendingInfo)
      val refundTxF = createRefundTx(fundingSpendingInfo)

      cetWinLocalF.foreach(cet =>
        logger.info(s"CET Win Local: ${cet._1.hex}\n"))
      cetLoseLocalF.foreach(cet =>
        logger.info(s"CET Lose Local: ${cet._1.hex}\n"))
      cetWinRemoteF.foreach(cet =>
        logger.info(s"CET Win Remote: ${cet._1.hex}\n"))
      cetLoseRemoteF.foreach(cet =>
        logger.info(s"CET Lose Remote: ${cet._1.hex}\n"))
      refundTxF.foreach(refundTx =>
        logger.info(s"Refund Tx: ${refundTx.hex}\n"))

      for {
        (cetWinLocal, cetWinLocalWitness) <- cetWinLocalF
        (cetLoseLocal, cetLoseLocalWitness) <- cetLoseLocalF
        (cetWinRemote, cetWinRemoteWitness) <- cetWinRemoteF
        (cetLoseRemote, cetLoseRemoteWitness) <- cetLoseRemoteF
        refundTx <- refundTxF
        fundingTx <- createFundingTransaction
      } yield {
        SetupDLC(
          fundingTx = fundingTx,
          fundingSpendingInfo = fundingSpendingInfo,
          cetWinLocal = cetWinLocal,
          cetWinLocalWitness = cetWinLocalWitness,
          cetLoseLocal = cetLoseLocal,
          cetLoseLocalWitness = cetLoseLocalWitness,
          cetWinRemote = cetWinRemote,
          cetWinRemoteWitness = cetWinRemoteWitness,
          cetLoseRemote = cetLoseRemote,
          cetLoseRemoteWitness = cetLoseRemoteWitness,
          refundTx = refundTx
        )
      }
    }
  }

  def constructClosingTx(
      privKey: ECPrivateKey,
      spendingInfo: BitcoinUTXOSpendingInfoFull,
      isWin: Boolean,
      spendsToLocal: Boolean): Future[Transaction] = {
    // If isToLocal, use payout as value, otherwise subtract fee
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
                 fundingSpendingInfo,
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

      // Pick the CET to use and payout by checking which message was signed
      val (cet, extCetPrivKey, remoteExtCetPubKey, cetScriptWitness) =
        if (sigForWin) {
          (cetWinLocal, cetWinPrivKey, cetRemoteWinPubKey, cetWinLocalWitness)
        } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
          (cetLoseLocal,
           cetLosePrivKey,
           cetRemoteLosePubKey,
           cetLoseLocalWitness)
        } else {
          throw new IllegalStateException(
            "Signature does not correspond to either possible outcome!")
        }

      val cetPrivKey = extCetPrivKey.deriveChildPrivKey(UInt32.zero).key
      val remoteCetPubKey =
        remoteExtCetPubKey.deriveChildPubKey(UInt32(2)).get.key

      // The prefix other refers to remote if local == true and local otherwise
      val output = cet.outputs.head
      val remoteOutput = cet.outputs.last

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
        signers = Vector(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = cetScriptWitness,
        conditionalPath = ConditionalPath.nonNestedTrue
      )

      val remoteCetSpendingInfo = P2WPKHV0SpendingInfo(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.one),
        amount = remoteOutput.value,
        scriptPubKey =
          remoteOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
        signer = ???,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(remoteCetPubKey)
      )

      val localSpendingTxF = constructClosingTx(finalPrivKey,
                                                cetSpendingInfo,
                                                isWin = sigForWin,
                                                spendsToLocal = true)

      localSpendingTxF.map { localSpendingTx =>
        DLCOutcome(
          fundingTx = fundingTx,
          cet = cet,
          localClosingTx = localSpendingTx,
          remoteClosingTx = ???,
          fundingUtxos = fundingUtxos,
          fundingSpendingInfo = fundingSpendingInfo,
          localCetSpendingInfo = cetSpendingInfo,
          remoteCetSpendingInfo = remoteCetSpendingInfo
        )
      }
    }
  }

  /** Constructs and executes on the justice spending branch of a DLC
    * where a published CET has timed out.
    *
    * @return Each transaction published and its spending info
    */
  def executeJusticeDLC(
      dlcSetup: SetupDLC,
      timedOutCET: Transaction): Future[DLCOutcome] = {
    val justiceOutput = timedOutCET.outputs.head
    val normalOutput = timedOutCET.outputs.last

    val (extCetPrivKey, cetScriptWitness) =
      if (timedOutCET == dlcSetup.cetWinRemote) {
        (cetWinPrivKey, dlcSetup.cetWinRemoteWitness)
      } else {
        (cetLosePrivKey, dlcSetup.cetLoseRemoteWitness)
      }

    val cetPrivKeyJustice = extCetPrivKey.deriveChildPrivKey(UInt32.one).key
    val cetPrivKeyToRemote = extCetPrivKey.deriveChildPrivKey(UInt32(2)).key

    val justiceSpendingInfo = P2WSHV0SpendingInfoFull(
      outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.zero),
      amount = justiceOutput.value,
      scriptPubKey = justiceOutput.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
      signers = Vector(cetPrivKeyJustice),
      hashType = HashType.sigHashAll,
      scriptWitness = cetScriptWitness,
      conditionalPath = ConditionalPath.nonNestedFalse
    )

    val normalSpendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.one),
      amount = normalOutput.value,
      scriptPubKey = normalOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = cetPrivKeyToRemote,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetPrivKeyToRemote.publicKey)
    )

    val isWin: Boolean = timedOutCET == dlcSetup.cetLoseRemote

    val justiceSpendingTxF =
      constructClosingTx(privKey = finalPrivKey,
                         spendingInfo = justiceSpendingInfo,
                         isWin = isWin,
                         spendsToLocal = false)
    val normalSpendingTxF =
      constructClosingTx(privKey = finalPrivKey,
                         spendingInfo = normalSpendingInfo,
                         isWin = isWin,
                         spendsToLocal = false)

    justiceSpendingTxF.flatMap { justiceSpendingTx =>
      normalSpendingTxF.map { normalSpendingTx =>
        // Note we misuse DLCOutcome a little here since there is no local and remote
        DLCOutcome(
          fundingTx = dlcSetup.fundingTx,
          cet = timedOutCET,
          localClosingTx = justiceSpendingTx,
          remoteClosingTx = normalSpendingTx,
          fundingUtxos = fundingUtxos,
          fundingSpendingInfo = dlcSetup.fundingSpendingInfo,
          localCetSpendingInfo = justiceSpendingInfo,
          remoteCetSpendingInfo = normalSpendingInfo
        )
      }
    }
  }

  /** Constructs and executes on the refund spending branch of a DLC
    *
    * @return Each transaction published and its spending info
    */
  def executeRefundDLC(dlcSetup: SetupDLC): Future[DLCOutcome] = {
    val SetupDLC(fundingTx,
                 fundingSpendingInfo,
                 _,
                 _,
                 _,
                 _,
                 _,
                 _,
                 _,
                 _,
                 refundTx) =
      dlcSetup

    val localOutput = refundTx.outputs.head
    val remoteOutput = refundTx.outputs.last

    val localRefundSpendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, UInt32.zero),
      amount = localOutput.value,
      scriptPubKey = localOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = cetRefundPrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetRefundPrivKey.publicKey)
    )

    val remoteRefundSpendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, UInt32.one),
      amount = remoteOutput.value,
      scriptPubKey = remoteOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = ???,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetRemoteRefundPubKey)
    )

    val localSpendingTxF = constructClosingTx(finalPrivKey,
                                              localRefundSpendingInfo,
                                              isWin = false,
                                              spendsToLocal = false)

    localSpendingTxF.map { localSpendingTx =>
      DLCOutcome(
        fundingTx = fundingTx,
        cet = refundTx,
        localClosingTx = localSpendingTx,
        remoteClosingTx = ???,
        fundingUtxos = fundingUtxos,
        fundingSpendingInfo = fundingSpendingInfo,
        localCetSpendingInfo = localRefundSpendingInfo,
        remoteCetSpendingInfo = remoteRefundSpendingInfo
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

  /** Subtracts the estimated fee by removing from each output with a specified spk evenly */
  def subtractFeeFromOutputs(
      txBuilder: BitcoinTxBuilder,
      spks: Vector[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[BitcoinTxBuilder] = {
    txBuilder.unsignedTx.flatMap { tx =>
      val fee = txBuilder.feeRate.calc(tx)

      val outputs = txBuilder.destinations.zipWithIndex.filter {
        case (output, _) => spks.contains(output.scriptPubKey)
      }
      val unchangedOutputs = txBuilder.destinations.zipWithIndex.filterNot {
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

      val allOuputsWithNew =
        (newOutputs ++ unchangedOutputs).sortBy(_._2).map(_._1)

      BitcoinTxBuilder(allOuputsWithNew,
                       txBuilder.utxoMap,
                       txBuilder.feeRate,
                       txBuilder.changeSPK,
                       txBuilder.network,
                       txBuilder.lockTimeOverrideOpt)
    }
  }
}

/** Contains all DLC transactions after initial setup. */
case class SetupDLC(
    fundingTx: Transaction,
    fundingSpendingInfo: P2WSHV0SpendingInfoFull,
    cetWinLocal: Transaction,
    cetWinLocalWitness: P2WSHWitnessV0,
    cetLoseLocal: Transaction,
    cetLoseLocalWitness: P2WSHWitnessV0,
    cetWinRemote: Transaction,
    cetWinRemoteWitness: P2WSHWitnessV0,
    cetLoseRemote: Transaction,
    cetLoseRemoteWitness: P2WSHWitnessV0,
    refundTx: Transaction
)

/** Contains all DLC transactions and the BitcoinUTXOSpendingInfos they use. */
case class DLCOutcome(
    fundingTx: Transaction,
    cet: Transaction,
    localClosingTx: Transaction,
    remoteClosingTx: Transaction,
    fundingUtxos: Vector[BitcoinUTXOSpendingInfoFull],
    fundingSpendingInfo: BitcoinUTXOSpendingInfoFull,
    localCetSpendingInfo: BitcoinUTXOSpendingInfoFull,
    remoteCetSpendingInfo: BitcoinUTXOSpendingInfoFull
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
