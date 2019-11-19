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
import org.bitcoins.core.util.CryptoUtil
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

// Two selfs: Local, Remote
// Two outcomes: Win, Lose
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
    network: BitcoinNetwork)(implicit ec: ExecutionContext) {

  val messageWin: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip.bytes

  val messageLose: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip.bytes

  val sigPubKeyWin: ECPublicKey =
    Schnorr.computePubKey(messageWin, preCommittedR, oraclePubKey)

  val sigPubKeyLose: ECPublicKey =
    Schnorr.computePubKey(messageLose, preCommittedR, oraclePubKey)

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

  def toLocalSPK(
      sigPubKey: ECPublicKey): MultiSignatureWithTimeoutScriptPubKey = {
    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetLocalPrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = ScriptNumber(timeout),
      scriptPubKey = P2PKHScriptPubKey(cetRemotePrivKey.publicKey))

    MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)
  }

  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: BitcoinUTXOSpendingInfo,
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

  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: BitcoinUTXOSpendingInfo,
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
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseLocal(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def executeDLC(oracleSigF: Future[SchnorrDigitalSignature]): Future[
    (Transaction, BitcoinUTXOSpendingInfo)] = {
    createFundingTransaction.flatMap { fundingTx =>
      println(s"Funding Transaction: ${fundingTx.hex}\n")

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

      val cetWinLocalF = createCETWinLocal(fundingSpendingInfo)
      val cetLoseLocalF = createCETLoseLocal(fundingSpendingInfo)
      val cetWinRemoteF = createCETWinRemote(fundingSpendingInfo)
      val cetLoseRemoteF = createCETLoseRemote(fundingSpendingInfo)

      cetWinLocalF.foreach(cet => println(s"CET Win Local: ${cet.hex}\n"))
      cetLoseLocalF.foreach(cet => println(s"CET Lose Local: ${cet.hex}\n"))
      cetWinRemoteF.foreach(cet => println(s"CET Win Remote: ${cet.hex}\n"))
      cetLoseRemoteF.foreach(cet => println(s"CET Lose Remote: ${cet.hex}\n"))

      // Publish funding tx

      oracleSigF.flatMap { oracleSig =>
        val cetLocalF =
          if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
            cetWinLocalF
          } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
            cetLoseLocalF
          } else {
            Future.failed(???)
          }

        cetLocalF.flatMap { cet =>
          val output = cet.outputs.head
          val cetSpendingInfo = ConditionalSpendingInfo(
            TransactionOutPoint(cet.txIdBE, UInt32.zero),
            output.value,
            output.scriptPubKey.asInstanceOf[ConditionalScriptPubKey],
            Vector(cetLocalPrivKey, ECPrivateKey(oracleSig.s)),
            HashType.sigHashAll,
            ConditionalPath.nonNestedTrue
          )

          val txBuilder = BitcoinTxBuilder(
            Vector(
              TransactionOutput(
                localWinPayout,
                P2PKHScriptPubKey(finalLocalPrivKey.publicKey))),
            Vector(cetSpendingInfo),
            feeRate,
            changeSPK,
            network
          )

          val spendingTxF = txBuilder.flatMap(subtractFeeAndSign)

          spendingTxF.foreach(tx => println(s"Closing Tx: ${tx.hex}"))

          // Publish tx

          spendingTxF.map(tx => (tx, cetSpendingInfo))
        }
      }
    }
  }
}
