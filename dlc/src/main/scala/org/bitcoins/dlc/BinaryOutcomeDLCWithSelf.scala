package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  ECPublicKey,
  Schnorr,
  SchnorrDigitalSignature
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
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
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  ConditionalPath,
  ConditionalSpendingInfo,
  MultiSignatureSpendingInfo
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
  * TODO: Make timeouts actually work
  * TODO: Add a time-locked refund transaction.
  *
  * @param outcomeWin The String whose hash is signed by the oracle in the Win case
  * @param outcomeLose The String whose hash is signed by the oracle in the Lose case
  * @param oraclePubKey The Oracle's permanent public key
  * @param preCommittedR The Oracle's one-time event-specific public key
  * @param fundingLocalPrivKey Local's funding private key
  * @param fundingRemotePrivKey Remote's funding private key
  * @param cetLocalPrivKey Local's CET private key
  * @param cetRemotePrivKey Remote's CET private key
  * @param finalLocalPrivKey Local's closing private key
  * @param finalRemotePrivKey Remote's closing private key
  * @param localInput Local's total collateral contribution
  * @param remoteInput Remote's total collateral contribution
  * @param localFundingUtxos Local's funding BitcoinUTXOSpendingInfo collection
  * @param remoteFundingUtxos Remote's funding BitcoinUTXOSpendingInfo collection
  * @param localWinPayout Local's payout in the Win case
  * @param remoteWinPayout Remote's payout in the Win case (in which Remote loses)
  * @param localLosePayout Local's payout in the Lose case
  * @param remoteLosePayout Remote's payout in the Lose case (in which Remote wins)
  * @param timeout The CLTV timeout in milliseconds used in all CETs
  * @param feeRate The predicted fee rate used for all transactions
  * @param changeSPK The place-holder change ScriptPubKey used for all transactions
  */
case class BinaryOutcomeDLCWithSelf(
    outcomeWin: String,
    outcomeLose: String,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey,
    fundingLocalPrivKey: ECPrivateKey,
    fundingRemotePrivKey: ECPrivateKey,
    cetLocalPrivKey: ECPrivateKey,
    cetRemotePrivKey: ECPrivateKey,
    finalLocalPrivKey: ECPrivateKey,
    finalRemotePrivKey: ECPrivateKey,
    localInput: CurrencyUnit,
    remoteInput: CurrencyUnit,
    localFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    remoteFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    localWinPayout: CurrencyUnit,
    remoteWinPayout: CurrencyUnit,
    localLosePayout: CurrencyUnit,
    remoteLosePayout: CurrencyUnit,
    timeout: Int,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

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

  /** Total funding amount */
  private val totalInput = localInput + remoteInput
  private val fundingUtxos = localFundingUtxos ++ remoteFundingUtxos

  val fundingLocalPubKey: ECPublicKey = fundingLocalPrivKey.publicKey
  val fundingRemotePubKey: ECPublicKey = fundingRemotePrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(2,
                               Vector(fundingLocalPubKey, fundingRemotePubKey))
  }

  /** Subtracts the estimated fee from the last output's value */
  private def subtractFeeAndSign(
      txBuilder: BitcoinTxBuilder): Future[Transaction] = {
    txBuilder.unsignedTx.flatMap { tx =>
      val fee = feeRate.calc(tx)
      val output = txBuilder.destinations.last
      val newOutput = TransactionOutput(output.value - fee, output.scriptPubKey)
      val newBuilder =
        BitcoinTxBuilder(txBuilder.destinations.dropRight(1).:+(newOutput),
                         txBuilder.utxoMap,
                         feeRate,
                         changeSPK,
                         network)

      newBuilder.flatMap(_.sign)
    }
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
    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetLocalPrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = ScriptNumber(timeout),
      scriptPubKey = P2PKHScriptPubKey(cetRemotePrivKey.publicKey))

    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, toLocalSPK)
    val feeSoFar = totalInput - fundingSpendingInfo.output.value
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout - feeSoFar,
                        P2PKHScriptPubKey(cetRemotePrivKey.publicKey))

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

    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetRemotePrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = ScriptNumber(timeout),
      scriptPubKey = P2PKHScriptPubKey(cetLocalPrivKey.publicKey))

    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout, toLocalSPK)
    val feeSoFar = totalInput - fundingSpendingInfo.output.value
    val toRemote: TransactionOutput =
      TransactionOutput(localPayout - feeSoFar,
                        P2PKHScriptPubKey(cetLocalPrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

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

  /** Constructs, signs and outputs the funding tx, all four CETs and the closing tx
    * given the oracle's signature (can be executed for either Local or Remote).
    *
    * @return The closing transaction and the UTXOSpendingInfo for the CET it spends.
    */
  def executeDLC(
      oracleSigF: Future[SchnorrDigitalSignature],
      local: Boolean): Future[(Transaction, BitcoinUTXOSpendingInfo)] = {
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

      cetWinLocalF.foreach(cet => logger.info(s"CET Win Local: ${cet.hex}\n"))
      cetLoseLocalF.foreach(cet => logger.info(s"CET Lose Local: ${cet.hex}\n"))
      cetWinRemoteF.foreach(cet => logger.info(s"CET Win Remote: ${cet.hex}\n"))
      cetLoseRemoteF.foreach(cet =>
        logger.info(s"CET Lose Remote: ${cet.hex}\n"))

      // TODO Publish funding tx

      oracleSigF.flatMap { oracleSig =>
        // Pick the CET to use and payout by checking which message was signed
        val (cetF, payout) =
          if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
            if (local) {
              (cetWinLocalF, localWinPayout)
            } else {
              (cetWinRemoteF, remoteWinPayout)
            }
          } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
            if (local) {
              (cetLoseLocalF, localLosePayout)
            } else {
              (cetLoseRemoteF, remoteLosePayout)
            }
          } else {
            (Future.failed(???), ???)
          }

        cetF.flatMap { cet =>
          // TODO Publish CET

          val cetPrivKey = if (local) {
            cetLocalPrivKey
          } else {
            cetRemotePrivKey
          }

          val output = cet.outputs.head

          // Spend the true case on the correct CET
          val cetSpendingInfo = ConditionalSpendingInfo(
            TransactionOutPoint(cet.txIdBE, UInt32.zero),
            output.value,
            output.scriptPubKey.asInstanceOf[ConditionalScriptPubKey],
            Vector(cetPrivKey, ECPrivateKey(oracleSig.s)),
            HashType.sigHashAll,
            ConditionalPath.nonNestedTrue
          )

          val finalPrivKey = if (local) {
            finalLocalPrivKey
          } else {
            finalRemotePrivKey
          }

          // Construct Closing Transaction
          val txBuilder = BitcoinTxBuilder(
            Vector(
              TransactionOutput(payout,
                                P2PKHScriptPubKey(finalPrivKey.publicKey))),
            Vector(cetSpendingInfo),
            feeRate,
            changeSPK,
            network
          )

          val spendingTxF = txBuilder.flatMap(subtractFeeAndSign)

          spendingTxF.foreach(tx => logger.info(s"Closing Tx: ${tx.hex}"))

          // TODO Publish tx

          spendingTxF.map(tx => (tx, cetSpendingInfo))
        }
      }
    }
  }
}
