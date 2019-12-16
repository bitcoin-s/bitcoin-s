package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
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
  ConditionalScriptPubKey,
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  P2PKHScriptPubKey,
  ScriptPubKey
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
  ConditionalSpendingInfo,
  MultiSignatureSpendingInfo,
  P2PKHSpendingInfo
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
  * @param changeSPK The place-holder change ScriptPubKey used for all transactions
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
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  import BinaryOutcomeDLCWithSelf.subtractFeeAndSign

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

  /** Total funding amount */
  private val totalInput = localInput + remoteInput

  /** Remote's payout in the Win case (in which Remote loses) */
  val remoteWinPayout: CurrencyUnit = totalInput - localWinPayout

  /** Remote's payout in the Lose case (in which Remote wins) */
  val remoteLosePayout: CurrencyUnit = totalInput - localLosePayout

  private val fundingUtxos = localFundingUtxos ++ remoteFundingUtxos

  val fundingLocalPubKey: ECPublicKey = fundingLocalPrivKey.publicKey
  val fundingRemotePubKey: ECPublicKey = fundingRemotePrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(2,
                               Vector(fundingLocalPubKey, fundingRemotePubKey))
  }

  def createFundingTransaction: Future[Transaction] = {
    val output: TransactionOutput =
      TransactionOutput(totalInput, fundingSPK)

    val outputs: Vector[TransactionOutput] = Vector(output)
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs, fundingUtxos, feeRate, changeSPK, network)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  /** Constructs Local's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: MultiSignatureSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
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

    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(localToLocalPrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = timeout.toScriptNumber,
      scriptPubKey = P2PKHScriptPubKey(remoteToLocalPrivKey.publicKey))

    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, toLocalSPK)
    val feeSoFar = totalInput - fundingSpendingInfo.output.value
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout - feeSoFar,
                        P2PKHScriptPubKey(toRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  /** Constructs Remote's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: MultiSignatureSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
    val (cetLocalPrivKey, cetRemotePrivKey) = if (sigPubKey == sigPubKeyWin) {
      (cetLocalWinPrivKey, cetRemoteWinPrivKey)
    } else {
      (cetLocalLosePrivKey, cetRemoteLosePrivKey)
    }

    val remoteToLocalPrivKey =
      cetRemotePrivKey.deriveChildPrivKey(UInt32.zero).key
    val localToLocalPrivKey = cetLocalPrivKey.deriveChildPrivKey(UInt32.one).key
    val toRemotePrivKey = cetLocalPrivKey.deriveChildPrivKey(UInt32(2)).key

    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(remoteToLocalPrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = timeout.toScriptNumber,
      scriptPubKey = P2PKHScriptPubKey(localToLocalPrivKey.publicKey))

    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout, toLocalSPK)
    val feeSoFar = totalInput - fundingSpendingInfo.output.value
    val toRemote: TransactionOutput =
      TransactionOutput(localPayout - feeSoFar,
                        P2PKHScriptPubKey(toRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  /** Constructs the (time-locked) refund transaction for when the oracle disappears
    * or signs an unknown message.
    * Note that both parties have the same refund transaction.
    */
  def createRefundTx(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    val toLocalValueNotSat =
      (fundingSpendingInfo.amount * localInput).satoshis.toLong / totalInput.satoshis.toLong
    val toLocalValue = Satoshis(Int64(toLocalValueNotSat))
    val toRemoteValue = fundingSpendingInfo.amount - toLocalValue

    val toLocal = TransactionOutput(
      toLocalValue,
      P2PKHScriptPubKey(cetLocalRefundPrivKey.publicKey))
    val toRemote = TransactionOutput(
      toRemoteValue,
      P2PKHScriptPubKey(cetRemoteRefundPrivKey.publicKey))

    val outputs = Vector(toLocal, toRemote)
    val txBuilderF = BitcoinTxBuilder(outputs,
                                      Vector(fundingSpendingInfo),
                                      feeRate,
                                      changeSPK,
                                      network,
                                      timeout.toUInt32)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  def createCETWinLocal(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseLocal(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
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
      val fundingSpendingInfo = MultiSignatureSpendingInfo(
        outPoint = TransactionOutPoint(fundingTxId, UInt32.zero),
        amount = output.value,
        scriptPubKey =
          output.scriptPubKey.asInstanceOf[MultiSignatureScriptPubKey],
        signers = Vector(fundingLocalPrivKey, fundingRemotePrivKey),
        hashType = HashType.sigHashAll
      )

      // Construct all CETs
      val cetWinLocalF = createCETWinLocal(fundingSpendingInfo)
      val cetLoseLocalF = createCETLoseLocal(fundingSpendingInfo)
      val cetWinRemoteF = createCETWinRemote(fundingSpendingInfo)
      val cetLoseRemoteF = createCETLoseRemote(fundingSpendingInfo)
      val refundTxF = createRefundTx(fundingSpendingInfo)

      cetWinLocalF.foreach(cet => logger.info(s"CET Win Local: ${cet.hex}\n"))
      cetLoseLocalF.foreach(cet => logger.info(s"CET Lose Local: ${cet.hex}\n"))
      cetWinRemoteF.foreach(cet => logger.info(s"CET Win Remote: ${cet.hex}\n"))
      cetLoseRemoteF.foreach(cet =>
        logger.info(s"CET Lose Remote: ${cet.hex}\n"))
      refundTxF.foreach(refundTx =>
        logger.info(s"Refund Tx: ${refundTx.hex}\n"))

      for {
        cetWinLocal <- cetWinLocalF
        cetLoseLocal <- cetLoseLocalF
        cetWinRemote <- cetWinRemoteF
        cetLoseRemote <- cetLoseRemoteF
        refundTx <- refundTxF
      } yield {
        SetupDLC(
          fundingTx = fundingTx,
          fundingSpendingInfo = fundingSpendingInfo,
          cetWinLocal = cetWinLocal,
          cetLoseLocal = cetLoseLocal,
          cetWinRemote = cetWinRemote,
          cetLoseRemote = cetLoseRemote,
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
      Vector(
        TransactionOutput(spendingInfo.output.value,
                          P2PKHScriptPubKey(privKey.publicKey))),
      Vector(spendingInfo),
      feeRate,
      changeSPK,
      network
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
                 cetLoseLocal,
                 cetWinRemote,
                 cetLoseRemote,
                 _) = dlcSetup

    oracleSigF.flatMap { oracleSig =>
      // Pick the CET to use and payout by checking which message was signed
      val (cet, extCetPrivKey, extOtherCetPrivKey) =
        if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
          if (local) {
            (cetWinLocal, cetLocalWinPrivKey, cetRemoteWinPrivKey)
          } else {
            (cetWinRemote, cetRemoteWinPrivKey, cetLocalWinPrivKey)
          }
        } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
          if (local) {
            (cetLoseLocal, cetLocalLosePrivKey, cetRemoteLosePrivKey)
          } else {
            (cetLoseRemote, cetRemoteLosePrivKey, cetLocalLosePrivKey)
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

      // Spend the true case on the correct CET
      val cetSpendingInfo = ConditionalSpendingInfo(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.zero),
        amount = output.value,
        scriptPubKey = output.scriptPubKey.asInstanceOf[ConditionalScriptPubKey],
        signers = Vector(cetPrivKey, ECPrivateKey(oracleSig.s)),
        hashType = HashType.sigHashAll,
        conditionalPath = ConditionalPath.nonNestedTrue
      )

      val otherCetSpendingInfo = P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(cet.txIdBE, UInt32.one),
        amount = otherOutput.value,
        scriptPubKey = otherOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
        signer = otherCetPrivKey,
        hashType = HashType.sigHashAll
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

    val extCetPrivKey = if (local) {
      if (timedOutCET == dlcSetup.cetWinRemote) {
        cetLocalWinPrivKey
      } else {
        cetLocalLosePrivKey
      }
    } else {
      if (timedOutCET == dlcSetup.cetWinLocal) {
        cetRemoteWinPrivKey
      } else {
        cetRemoteLosePrivKey
      }
    }

    val cetPrivKeyJustice = extCetPrivKey.deriveChildPrivKey(UInt32.one).key
    val cetPrivKeyToRemote = extCetPrivKey.deriveChildPrivKey(UInt32(2)).key

    val justiceSpendingInfo = ConditionalSpendingInfo(
      outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.zero),
      amount = justiceOutput.value,
      scriptPubKey =
        justiceOutput.scriptPubKey.asInstanceOf[ConditionalScriptPubKey],
      signers = Vector(cetPrivKeyJustice),
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.nonNestedFalse
    )

    val normalSpendingInfo = P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(timedOutCET.txIdBE, UInt32.one),
      amount = normalOutput.value,
      scriptPubKey = normalOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
      signer = cetPrivKeyToRemote,
      hashType = HashType.sigHashAll
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
    val SetupDLC(fundingTx, fundingSpendingInfo, _, _, _, _, refundTx) =
      dlcSetup

    val localOutput = refundTx.outputs.head
    val remoteOutput = refundTx.outputs.last

    val localRefundSpendingInfo = P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, UInt32.zero),
      amount = localOutput.value,
      scriptPubKey = localOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
      signer = cetLocalRefundPrivKey,
      hashType = HashType.sigHashAll
    )

    val remoteRefundSpendingInfo = P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(refundTx.txIdBE, UInt32.one),
      amount = remoteOutput.value,
      scriptPubKey = remoteOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
      signer = cetRemoteRefundPrivKey,
      hashType = HashType.sigHashAll
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
    txBuilder.unsignedTx.flatMap { tx =>
      val fee = txBuilder.feeRate.calc(tx)

      val outputs = txBuilder.destinations

      val feePerOutput = Satoshis(Int64(fee.satoshis.toLong / outputs.length))
      val feeRemainder = Satoshis(Int64(fee.satoshis.toLong % outputs.length))

      val newOutputsWithoutRemainder = outputs.map(output =>
        TransactionOutput(output.value - feePerOutput, output.scriptPubKey))
      val lastOutput = newOutputsWithoutRemainder.last
      val newLastOutput = TransactionOutput(lastOutput.value - feeRemainder,
                                            lastOutput.scriptPubKey)
      val newOutputs = newOutputsWithoutRemainder.dropRight(1).:+(newLastOutput)

      val newBuilder =
        BitcoinTxBuilder(newOutputs,
                         txBuilder.utxoMap,
                         txBuilder.feeRate,
                         txBuilder.changeSPK,
                         txBuilder.network,
                         txBuilder.lockTimeOverrideOpt)

      newBuilder.flatMap(_.sign)
    }
  }
}

/** Contains all DLC transactions after initial setup. */
case class SetupDLC(
    fundingTx: Transaction,
    fundingSpendingInfo: MultiSignatureSpendingInfo,
    cetWinLocal: Transaction,
    cetLoseLocal: Transaction,
    cetWinRemote: Transaction,
    cetLoseRemote: Transaction,
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
