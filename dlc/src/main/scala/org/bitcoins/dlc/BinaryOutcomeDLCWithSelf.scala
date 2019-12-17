package org.bitcoins.dlc

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrDigitalSignature
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.hd.{BIP32Node, BIP32Path}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStampWithFuture
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  EmptyScriptPubKey,
  MultiSignatureScriptPubKey,
  NonStandardIfConditionalScriptPubKey,
  P2PKHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  ScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  ConditionalPath,
  P2WPKHV0SpendingInfo,
  P2WSHV0SpendingInfo
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** This case class allows for the construction and execution of binary outcome
  * Discreet Log Contracts between one party and itself. In the future this will
  * be split amongst two separate parties. This change will likely require that
  * this class be largely altered.
  *
  * The "two parties", which are actually just one node taking both positions, are
  * referred to as Local and Remote. The two outcomes are called Win and Lose but
  * note that Win refers to the case where Local wins money and Remote loses money.
  * Likewise Lose refers to the case where Remote wins and Local loses money.
  *
  * @param outcomeWin The String whose hash is signed by the oracle in the Win case
  * @param outcomeLose The String whose hash is signed by the oracle in the Lose case
  * @param oraclePubKey The Oracle's permanent public key
  * @param preCommittedR The Oracle's one-time event-specific public key
  * @param localExtPrivKey Local's extended private key for this event
  * @param remoteExtPrivKey Remote's extended private key for this event
  * @param localInput Local's total collateral contribution
  * @param remoteInput Remote's total collateral contribution
  * @param localFundingUtxos Local's funding BitcoinUTXOSpendingInfo collection
  * @param remoteFundingUtxos Remote's funding BitcoinUTXOSpendingInfo collection
  * @param localWinPayout Local's payout in the Win case
  * @param localLosePayout Local's payout in the Lose case
  * @param timeout The CLTV timeout in milliseconds used in all CETs
  * @param feeRate The predicted fee rate used for all transactions
  * @param localChangeSPK Local's change ScriptPubKey used in the funding tx
  * @param remoteChangeSPK Remote's change ScriptPubKey used in the funding tx
  */
case class BinaryOutcomeDLCWithSelf(
    outcomeWin: String,
    outcomeLose: String,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey,
    localExtPrivKey: ExtPrivateKey,
    remoteExtPrivKey: ExtPrivateKey,
    localInput: CurrencyUnit,
    remoteInput: CurrencyUnit,
    localFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    remoteFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    localWinPayout: CurrencyUnit,
    localLosePayout: CurrencyUnit,
    timeout: BlockStampWithFuture,
    feeRate: FeeUnit,
    localChangeSPK: WitnessScriptPubKeyV0,
    remoteChangeSPK: WitnessScriptPubKeyV0,
    network: BitcoinNetwork)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  import BinaryOutcomeDLCWithSelf._

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

  val fundingLocalPrivKey: ECPrivateKey =
    localExtPrivKey.deriveChildPrivKey(UInt32(0)).key

  val fundingRemotePrivKey: ECPrivateKey =
    remoteExtPrivKey.deriveChildPrivKey(UInt32(0)).key

  val finalLocalPrivKey: ECPrivateKey =
    localExtPrivKey.deriveChildPrivKey(UInt32(2)).key

  val finalRemotePrivKey: ECPrivateKey =
    remoteExtPrivKey.deriveChildPrivKey(UInt32(2)).key

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

  val cetLocalRefundPrivKey: ECPrivateKey =
    cetExtPrivKey(localExtPrivKey, eventIndex = 0).key

  val cetLocalWinPrivKey: ExtPrivateKey =
    cetExtPrivKey(localExtPrivKey, winIndex)

  val cetLocalLosePrivKey: ExtPrivateKey =
    cetExtPrivKey(localExtPrivKey, loseIndex)

  val cetRemoteRefundPrivKey: ECPrivateKey =
    cetExtPrivKey(remoteExtPrivKey, eventIndex = 0).key

  val cetRemoteWinPrivKey: ExtPrivateKey =
    cetExtPrivKey(remoteExtPrivKey, winIndex)

  val cetRemoteLosePrivKey: ExtPrivateKey =
    cetExtPrivKey(remoteExtPrivKey, loseIndex)

  /** Total collateral amount */
  private val totalInput = localInput + remoteInput

  private val localFunding =
    localFundingUtxos.foldLeft(0L)(_ + _.amount.satoshis.toLong)
  private val remoteFunding =
    remoteFundingUtxos.foldLeft(0L)(_ + _.amount.satoshis.toLong)

  /** Remote's payout in the Win case (in which Remote loses) */
  val remoteWinPayout: CurrencyUnit = totalInput - localWinPayout

  /** Remote's payout in the Lose case (in which Remote wins) */
  val remoteLosePayout: CurrencyUnit = totalInput - localLosePayout

  /** This is only used as a placeholder and we use an invariant
    * when signing to ensure that this is never used.
    *
    * In the future, allowing this behavior should be done in TxBuilder.
    */
  private val emptyChangeSPK: ScriptPubKey = EmptyScriptPubKey

  private val fundingUtxos = localFundingUtxos ++ remoteFundingUtxos

  val fundingLocalPubKey: ECPublicKey = fundingLocalPrivKey.publicKey
  val fundingRemotePubKey: ECPublicKey = fundingRemotePrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(2,
                               Vector(fundingLocalPubKey, fundingRemotePubKey))
  }

  def createFundingTransaction: Future[Transaction] = {

    /* We need to commit to the CET's fee during the construction of
     * the funding transaction so that the CET outputs have the expected payouts.
     *
     * Our approach at the moment is to estimate CET size and, using feeRate,
     * approximate how much the fee will be on the cetWinLocal transaction. It
     * does not matter which CET we use since they are all the same structure/size
     * other than the refund case which is smaller.
     *
     * Once computed, we add that amount to the fundingOutput so it can be used for fees later.
     */
    val cetWinLocalF = createMockCET()
    val cetFeeF = cetWinLocalF.map(feeRate.calc)

    cetFeeF.flatMap { cetFee =>
      val halfCetFee = Satoshis(cetFee.satoshis.toLong / 2)

      val output: TransactionOutput =
        TransactionOutput(totalInput + cetFee, P2WSHWitnessSPKV0(fundingSPK))
      val localChange =
        TransactionOutput(Satoshis(localFunding) - localInput - halfCetFee,
                          localChangeSPK)
      val remoteChange =
        TransactionOutput(Satoshis(remoteFunding) - remoteInput - halfCetFee,
                          remoteChangeSPK)

      val outputs: Vector[TransactionOutput] =
        Vector(output, localChange, remoteChange)
      val txBuilderF: Future[BitcoinTxBuilder] =
        BitcoinTxBuilder(outputs,
                         fundingUtxos,
                         feeRate,
                         emptyChangeSPK,
                         network)

      txBuilderF.flatMap { txBuilder =>
        subtractFeeFromOutputsAndSign(txBuilder,
                                      Vector(localChangeSPK, remoteChangeSPK))
      }
    }
  }

  /** Constructs Local's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: P2WSHV0SpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      invariant: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean =
        noEmptyOutputs): Future[(Transaction, P2WSHWitnessV0)] = {
    val (cetLocalPrivKey, cetRemotePrivKey) = if (sigPubKey == sigPubKeyWin) {
      (cetLocalWinPrivKey, cetRemoteWinPrivKey)
    } else {
      (cetLocalLosePrivKey, cetRemoteLosePrivKey)
    }

    val localToLocalPrivKey =
      cetLocalPrivKey.deriveChildPrivKey(UInt32.zero).key
    val remoteToLocalPrivKey =
      cetRemotePrivKey.deriveChildPrivKey(UInt32.one).key
    val toRemotePrivKey = cetRemotePrivKey.deriveChildPrivKey(UInt32(2)).key

    val pubKeyBytes = NativeSecp256k1.pubKeyTweakAdd(
      sigPubKey.bytes.toArray,
      localToLocalPrivKey.bytes.toArray,
      true)
    val pubKey = ECPublicKey.fromBytes(ByteVector(pubKeyBytes))
    val oracleSPK = P2PKHScriptPubKey(pubKey)

    val timeoutSPK = CLTVScriptPubKey(
      locktime = timeout.toScriptNumber,
      scriptPubKey = P2PKHScriptPubKey(remoteToLocalPrivKey.publicKey))

    val toLocalSPK = NonStandardIfConditionalScriptPubKey(oracleSPK, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout,
                        P2WPKHWitnessSPKV0(toRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       emptyChangeSPK,
                       network)

    txBuilderF
      .flatMap(_.sign(invariant))
      .map((_, P2WSHWitnessV0(toLocalSPK)))
  }

  /** Constructs Remote's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: P2WSHV0SpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      invariant: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean =
        noEmptyOutputs): Future[(Transaction, P2WSHWitnessV0)] = {
    val (cetLocalPrivKey, cetRemotePrivKey) = if (sigPubKey == sigPubKeyWin) {
      (cetLocalWinPrivKey, cetRemoteWinPrivKey)
    } else {
      (cetLocalLosePrivKey, cetRemoteLosePrivKey)
    }

    val remoteToLocalPrivKey =
      cetRemotePrivKey.deriveChildPrivKey(UInt32.zero).key
    val localToLocalPrivKey = cetLocalPrivKey.deriveChildPrivKey(UInt32.one).key
    val toRemotePrivKey = cetLocalPrivKey.deriveChildPrivKey(UInt32(2)).key

    val pubKeyBytes = NativeSecp256k1.pubKeyTweakAdd(
      sigPubKey.bytes.toArray,
      remoteToLocalPrivKey.bytes.toArray,
      true)
    val pubKey = ECPublicKey.fromBytes(ByteVector(pubKeyBytes))
    val oracleSPK = P2PKHScriptPubKey(pubKey)

    val timeoutSPK = CLTVScriptPubKey(
      locktime = timeout.toScriptNumber,
      scriptPubKey = P2PKHScriptPubKey(localToLocalPrivKey.publicKey))

    val toLocalSPK = NonStandardIfConditionalScriptPubKey(oracleSPK, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout, P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(localPayout,
                        P2WPKHWitnessSPKV0(toRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       emptyChangeSPK,
                       network)

    txBuilderF
      .flatMap(_.sign(invariant))
      .map((_, P2WSHWitnessV0(toLocalSPK)))
  }

  /** Constructs the (time-locked) refund transaction for when the oracle disappears
    * or signs an unknown message.
    * Note that both parties have the same refund transaction.
    */
  def createRefundTx(
      fundingSpendingInfo: P2WSHV0SpendingInfo): Future[Transaction] = {
    val toLocalValueNotSat =
      (fundingSpendingInfo.amount * localInput).satoshis.toLong / totalInput.satoshis.toLong
    val toLocalValue = Satoshis(toLocalValueNotSat)
    val toRemoteValue = fundingSpendingInfo.amount - toLocalValue

    val toLocal = TransactionOutput(
      toLocalValue,
      P2WPKHWitnessSPKV0(cetLocalRefundPrivKey.publicKey))
    val toRemote = TransactionOutput(
      toRemoteValue,
      P2WPKHWitnessSPKV0(cetRemoteRefundPrivKey.publicKey))

    val outputs = Vector(toLocal, toRemote)
    val txBuilderF = BitcoinTxBuilder(outputs,
                                      Vector(fundingSpendingInfo),
                                      feeRate,
                                      emptyChangeSPK,
                                      network,
                                      timeout.toUInt32)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  private def createMockCET(): Future[Transaction] = {
    val emptyOutPoint =
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero)
    val dummySpendingInfo = P2WSHV0SpendingInfo(
      outPoint = emptyOutPoint,
      amount = totalInput * 2, // Any amount significantly > totalInput should be valid
      scriptPubKey = P2WSHWitnessSPKV0(fundingSPK),
      signers = Vector(fundingLocalPrivKey, fundingRemotePrivKey),
      hashType = HashType.sigHashAll,
      scriptWitness = P2WSHWitnessV0(fundingSPK),
      conditionalPath = ConditionalPath.NoConditionsLeft
    )

    val mockCETF = createCETLocal(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = dummySpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout,
      invariant = (_, _) => true
    )

    mockCETF.map(_._1)
  }

  def createCETWinLocal(fundingSpendingInfo: P2WSHV0SpendingInfo): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCETLocal(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseLocal(fundingSpendingInfo: P2WSHV0SpendingInfo): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCETLocal(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(fundingSpendingInfo: P2WSHV0SpendingInfo): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(fundingSpendingInfo: P2WSHV0SpendingInfo): Future[
    (Transaction, P2WSHWitnessV0)] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def setupDLC(): Future[SetupDLC] = {
    // Construct Funding Transaction
    createFundingTransaction.flatMap { fundingTx =>
      logger.info(s"Funding Transaction: ${fundingTx.hex}\n")

      val fundingTxId = fundingTx.txIdBE
      val output = fundingTx.outputs.head
      val fundingSpendingInfo = P2WSHV0SpendingInfo(
        outPoint = TransactionOutPoint(fundingTxId, UInt32.zero),
        amount = output.value,
        scriptPubKey = output.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
        signers = Vector(fundingLocalPrivKey, fundingRemotePrivKey),
        hashType = HashType.sigHashAll,
        scriptWitness = P2WSHWitnessV0(fundingSPK),
        conditionalPath = ConditionalPath.NoConditionsLeft
      )

      // Construct all CETs
      val cetWinLocalF = createCETWinLocal(fundingSpendingInfo)
      val cetLoseLocalF = createCETLoseLocal(fundingSpendingInfo)
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
      spendingInfo: BitcoinUTXOSpendingInfo,
      isLocal: Boolean): Future[Transaction] = {
    // Construct Closing Transaction
    val txBuilder = BitcoinTxBuilder(
      destinations = Vector(
        TransactionOutput(spendingInfo.output.value,
                          P2WPKHWitnessSPKV0(privKey.publicKey))),
      utxos = Vector(spendingInfo),
      feeRate = feeRate,
      changeSPK = emptyChangeSPK,
      network = network
    )

    val spendingTxF = txBuilder.flatMap(subtractFeeAndSign)

    spendingTxF.foreach(
      tx =>
        logger.info(
          s"${if (isLocal) "Local" else "Remote"} Closing Tx: ${tx.hex}"))

    spendingTxF
  }

  /** Constructs and executes on the unilateral spending branch of a DLC
    *
    * @return Each transaction published and its spending info
    */
  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSigF: Future[SchnorrDigitalSignature],
      local: Boolean): Future[DLCOutcome] = {
    val SetupDLC(fundingTx,
                 fundingSpendingInfo,
                 cetWinLocal,
                 cetWinLocalWitness,
                 cetLoseLocal,
                 cetLoseLocalWitness,
                 cetWinRemote,
                 cetWinRemoteWitness,
                 cetLoseRemote,
                 cetLoseRemoteWitness,
                 _) = dlcSetup

    oracleSigF.flatMap { oracleSig =>
      // Pick the CET to use and payout by checking which message was signed
      val (cet, extCetPrivKey, extOtherCetPrivKey, cetScriptWitness) =
        if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
          if (local) {
            (cetWinLocal,
             cetLocalWinPrivKey,
             cetRemoteWinPrivKey,
             cetWinLocalWitness)
          } else {
            (cetWinRemote,
             cetRemoteWinPrivKey,
             cetLocalWinPrivKey,
             cetWinRemoteWitness)
          }
        } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
          if (local) {
            (cetLoseLocal,
             cetLocalLosePrivKey,
             cetRemoteLosePrivKey,
             cetLoseLocalWitness)
          } else {
            (cetLoseRemote,
             cetRemoteLosePrivKey,
             cetLocalLosePrivKey,
             cetLoseRemoteWitness)
          }
        } else {
          throw new IllegalStateException(
            "Signature does not correspond to either possible outcome!")
        }

      val cetPrivKey = extCetPrivKey.deriveChildPrivKey(UInt32.zero).key
      val otherCetPrivKey = extOtherCetPrivKey.deriveChildPrivKey(UInt32(2)).key

      // The prefix other refers to remote if local == true and local otherwise
      val output = cet.outputs.head
      val otherOutput = cet.outputs.last

      val privKeyBytes = NativeSecp256k1.privKeyTweakAdd(
        cetPrivKey.bytes.toArray,
        oracleSig.s.toArray
      )
      val privKey = ECPrivateKey.fromBytes(ByteVector(privKeyBytes))

      // Spend the true case on the correct CET
      val cetSpendingInfo = P2WSHV0SpendingInfo(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.zero),
        amount = output.value,
        scriptPubKey = output.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0],
        signers = Vector(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = cetScriptWitness,
        conditionalPath = ConditionalPath.nonNestedTrue
      )

      val otherCetSpendingInfo = P2WPKHV0SpendingInfo(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.one),
        amount = otherOutput.value,
        scriptPubKey = otherOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
        signer = otherCetPrivKey,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(otherCetPrivKey.publicKey)
      )

      val (localCetSpendingInfo, remoteCetSpendingInfo) = if (local) {
        (cetSpendingInfo, otherCetSpendingInfo)
      } else {
        (otherCetSpendingInfo, cetSpendingInfo)
      }

      val localSpendingTxF = constructClosingTx(finalLocalPrivKey,
                                                localCetSpendingInfo,
                                                isLocal = true)
      val remoteSpendingTxF = constructClosingTx(finalRemotePrivKey,
                                                 remoteCetSpendingInfo,
                                                 isLocal = false)

      localSpendingTxF.flatMap { localSpendingTx =>
        remoteSpendingTxF.map { remoteSpendingTx =>
          DLCOutcome(
            fundingTx = fundingTx,
            cet = cet,
            localClosingTx = localSpendingTx,
            remoteClosingTx = remoteSpendingTx,
            fundingUtxos = fundingUtxos,
            fundingSpendingInfo = fundingSpendingInfo,
            localCetSpendingInfo = localCetSpendingInfo,
            remoteCetSpendingInfo = remoteCetSpendingInfo
          )
        }
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
      timedOutCET: Transaction,
      local: Boolean): Future[DLCOutcome] = {
    val justiceOutput = timedOutCET.outputs.head
    val normalOutput = timedOutCET.outputs.last

    val (extCetPrivKey, cetScriptWitness) = if (local) {
      if (timedOutCET == dlcSetup.cetWinRemote) {
        (cetLocalWinPrivKey, dlcSetup.cetWinRemoteWitness)
      } else {
        (cetLocalLosePrivKey, dlcSetup.cetLoseRemoteWitness)
      }
    } else {
      if (timedOutCET == dlcSetup.cetWinLocal) {
        (cetRemoteWinPrivKey, dlcSetup.cetWinLocalWitness)
      } else {
        (cetRemoteLosePrivKey, dlcSetup.cetLoseLocalWitness)
      }
    }

    val cetPrivKeyJustice = extCetPrivKey.deriveChildPrivKey(UInt32.one).key
    val cetPrivKeyToRemote = extCetPrivKey.deriveChildPrivKey(UInt32(2)).key

    val justiceSpendingInfo = P2WSHV0SpendingInfo(
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

    val finalPrivKey = if (local) {
      finalLocalPrivKey
    } else {
      finalRemotePrivKey
    }

    val justiceSpendingTxF =
      constructClosingTx(finalPrivKey, justiceSpendingInfo, local)
    val normalSpendingTxF =
      constructClosingTx(finalPrivKey, normalSpendingInfo, local)

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
      signer = cetLocalRefundPrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetLocalRefundPrivKey.publicKey)
    )

    val remoteRefundSpendingInfo = P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, UInt32.one),
      amount = remoteOutput.value,
      scriptPubKey = remoteOutput.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = cetRemoteRefundPrivKey,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(cetRemoteRefundPrivKey.publicKey)
    )

    val localSpendingTxF = constructClosingTx(finalLocalPrivKey,
                                              localRefundSpendingInfo,
                                              isLocal = true)
    val remoteSpendingTxF = constructClosingTx(finalRemotePrivKey,
                                               remoteRefundSpendingInfo,
                                               isLocal = false)

    localSpendingTxF.flatMap { localSpendingTx =>
      remoteSpendingTxF.map { remoteSpendingTx =>
        DLCOutcome(
          fundingTx = fundingTx,
          cet = refundTx,
          localClosingTx = localSpendingTx,
          remoteClosingTx = remoteSpendingTx,
          fundingUtxos = fundingUtxos,
          fundingSpendingInfo = fundingSpendingInfo,
          localCetSpendingInfo = localRefundSpendingInfo,
          remoteCetSpendingInfo = remoteRefundSpendingInfo
        )
      }
    }
  }
}

object BinaryOutcomeDLCWithSelf {

  /** Subtracts the estimated fee by removing from each output evenly */
  def subtractFeeAndSign(txBuilder: BitcoinTxBuilder)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val spks = txBuilder.destinations.toVector.map(_.scriptPubKey)

    subtractFeeFromOutputsAndSign(txBuilder, spks)
  }

  // This invariant ensures that emptyChangeSPK is never used above
  val noEmptyOutputs: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean = {
    (_, tx) =>
      tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  /** Subtracts the estimated fee by removing from each output with a specified spk evenly */
  def subtractFeeFromOutputsAndSign(
      txBuilder: BitcoinTxBuilder,
      spks: Vector[ScriptPubKey])(
      implicit ec: ExecutionContext): Future[Transaction] = {
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

      val newBuilder =
        BitcoinTxBuilder(allOuputsWithNew,
                         txBuilder.utxoMap,
                         txBuilder.feeRate,
                         txBuilder.changeSPK,
                         txBuilder.network,
                         txBuilder.lockTimeOverrideOpt)

      newBuilder.flatMap(_.sign(noEmptyOutputs))
    }
  }
}

/** Contains all DLC transactions after initial setup. */
case class SetupDLC(
    fundingTx: Transaction,
    fundingSpendingInfo: P2WSHV0SpendingInfo,
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
    fundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    fundingSpendingInfo: BitcoinUTXOSpendingInfo,
    localCetSpendingInfo: BitcoinUTXOSpendingInfo,
    remoteCetSpendingInfo: BitcoinUTXOSpendingInfo
)
