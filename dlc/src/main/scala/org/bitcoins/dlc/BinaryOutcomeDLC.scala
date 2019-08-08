package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{ECPublicKey, Schnorr}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  P2PKHScriptPubKey,
  P2WSHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

case class BinaryOutcomeDLC(
    outcome1: String,
    outcome2: String,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey) {

  val message1: ByteVector =
    CryptoUtil.sha256(ByteVector(outcome1.getBytes)).bytes

  val message2: ByteVector =
    CryptoUtil.sha256(ByteVector(outcome2.getBytes)).bytes

  val sigPubKey1: ECPublicKey =
    Schnorr.computePubKey(message1, preCommittedR, oraclePubKey)

  val sigPubKey2: ECPublicKey =
    Schnorr.computePubKey(message2, preCommittedR, oraclePubKey)

  def createFundingTransaction(
      key1: ECPublicKey,
      key2: ECPublicKey,
      totalAmount: CurrencyUnit,
      utxos: Vector[BitcoinUTXOSpendingInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val spk: ScriptPubKey =
      MultiSignatureScriptPubKey(2, Vector(key1, key2))

    val output: TransactionOutput =
      TransactionOutput(totalAmount, P2WSHWitnessSPKV0(spk))

    val outputs: Vector[TransactionOutput] = Vector(output)
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs, utxos, feeRate, changeSPK, network)

    txBuilderF.flatMap(_.unsignedTx)
  }

  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      localPubKey: ECPublicKey,
      remotePubKey: ECPublicKey,
      timeout: Int,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(localPubKey, sigPubKey),
      timeout = timeout,
      timeoutPubKey = remotePubKey)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, P2WSHWitnessSPKV0(toLocalSPK))
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout, P2PKHScriptPubKey(remotePubKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs, Vector(fundingTx), feeRate, changeSPK, network)

    txBuilderF.flatMap(_.unsignedTx)
  }

  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      localPubKey: ECPublicKey,
      remotePubKey: ECPublicKey,
      timeout: Int,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKey,
      fundingTx = fundingTx,
      localPayout = remotePayout,
      remotePayout = localPayout,
      localPubKey = remotePubKey,
      remotePubKey = localPubKey,
      timeout = timeout,
      feeRate = feeRate,
      changeSPK = changeSPK,
      network = network
    )
  }

  def createCET1Local(
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      localPubKey: ECPublicKey,
      remotePubKey: ECPublicKey,
      timeout: Int,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKey1,
      fundingTx = fundingTx,
      localPayout = localPayout,
      remotePayout = remotePayout,
      localPubKey = localPubKey,
      remotePubKey = remotePubKey,
      timeout = timeout,
      feeRate = feeRate,
      changeSPK = changeSPK,
      network = network
    )
  }

  def createCET2Local(
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      localPubKey: ECPublicKey,
      remotePubKey: ECPublicKey,
      timeout: Int,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKey2,
      fundingTx = fundingTx,
      localPayout = localPayout,
      remotePayout = remotePayout,
      localPubKey = localPubKey,
      remotePubKey = remotePubKey,
      timeout = timeout,
      feeRate = feeRate,
      changeSPK = changeSPK,
      network = network
    )
  }

  def createCET1Remote(
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      localPubKey: ECPublicKey,
      remotePubKey: ECPublicKey,
      timeout: Int,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKey1,
      fundingTx = fundingTx,
      localPayout = localPayout,
      remotePayout = remotePayout,
      localPubKey = localPubKey,
      remotePubKey = remotePubKey,
      timeout = timeout,
      feeRate = feeRate,
      changeSPK = changeSPK,
      network = network
    )
  }

  def createCET2Remote(
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit,
      localPubKey: ECPublicKey,
      remotePubKey: ECPublicKey,
      timeout: Int,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKey2,
      fundingTx = fundingTx,
      localPayout = localPayout,
      remotePayout = remotePayout,
      localPubKey = localPubKey,
      remotePubKey = remotePubKey,
      timeout = timeout,
      feeRate = feeRate,
      changeSPK = changeSPK,
      network = network
    )
  }

  // TODO: Deal with timeouts due to disappearing oracle
}
