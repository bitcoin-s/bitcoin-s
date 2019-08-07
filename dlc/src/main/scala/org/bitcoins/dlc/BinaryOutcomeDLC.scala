package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  ECDigitalSignature,
  ECPublicKey,
  Schnorr,
  SchnorrDigitalSignature,
  Sign
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  P2PKHScriptPubKey,
  P2WSHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
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
    preCommittedR: ECPublicKey,
    localPubKey: ECPublicKey,
    remotePubKey: ECPublicKey,
    localInput: CurrencyUnit,
    remoteInput: CurrencyUnit,
    localFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    remoteFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    localPayout1: CurrencyUnit,
    remotePayout1: CurrencyUnit,
    localPayout2: CurrencyUnit,
    remotePayout2: CurrencyUnit,
    timeout: Int,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork)(implicit ec: ExecutionContext) {

  val message1: ByteVector =
    CryptoUtil.sha256(ByteVector(outcome1.getBytes)).bytes

  val message2: ByteVector =
    CryptoUtil.sha256(ByteVector(outcome2.getBytes)).bytes

  val sigPubKey1: ECPublicKey =
    Schnorr.computePubKey(message1, preCommittedR, oraclePubKey)

  val sigPubKey2: ECPublicKey =
    Schnorr.computePubKey(message2, preCommittedR, oraclePubKey)

  private val totalInput = localInput + remoteInput
  private val fundingUtxos = localFundingUtxos ++ remoteFundingUtxos

  def createFundingTransaction(
      key1: ECPublicKey,
      key2: ECPublicKey): Future[Transaction] = {
    val spk: ScriptPubKey =
      MultiSignatureScriptPubKey(2, Vector(key1, key2))

    val output: TransactionOutput =
      TransactionOutput(totalInput, P2WSHWitnessSPKV0(spk))

    val outputs: Vector[TransactionOutput] = Vector(output)
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs, fundingUtxos, feeRate, changeSPK, network)

    txBuilderF.flatMap(_.unsignedTx)
  }

  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingTx: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
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
      remotePayout: CurrencyUnit): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKey,
      fundingTx = fundingTx,
      localPayout = remotePayout,
      remotePayout = localPayout
    )
  }

  def createCET1Local(
      fundingTx: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKey1,
      fundingTx = fundingTx,
      localPayout = localPayout1,
      remotePayout = remotePayout1
    )
  }

  def createCET2Local(
      fundingTx: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKey2,
      fundingTx = fundingTx,
      localPayout = localPayout2,
      remotePayout = remotePayout2
    )
  }

  def createCET1Remote(
      fundingTx: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKey1,
      fundingTx = fundingTx,
      localPayout = localPayout1,
      remotePayout = remotePayout1
    )
  }

  def createCET2Remote(
      fundingTx: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKey2,
      fundingTx = fundingTx,
      localPayout = localPayout2,
      remotePayout = remotePayout2
    )
  }

  // TODO: Deal with timeouts due to disappearing oracle

  def executeDLC(
      localSign: Sign,
      remoteSig1F: Future[ECDigitalSignature],
      remoteSig2F: Future[ECDigitalSignature],
      oracleSigF: Future[SchnorrDigitalSignature]): Unit = {
    // construct funding tx
    val unsignedFundingTxF = createFundingTransaction(localPubKey, remotePubKey)

    unsignedFundingTxF.map { unsignedFundingTx =>
      val fundingTxId = unsignedFundingTx.txIdBE
      val unsignedFundingSpendingInfo =
        BitcoinUTXOSpendingInfo(
          outPoint = TransactionOutPoint(fundingTxId, UInt32(0)),
          output = unsignedFundingTx.outputs.head,
          signers = Vector.empty,
          redeemScriptOpt = None,
          scriptWitnessOpt = None,
          hashType = HashType.sigHashAll
        )

      // sign remote CETs
      val unsignedCet1RemoteF = createCET1Remote(
        fundingTx = unsignedFundingSpendingInfo
      )

      val sig1F = unsignedCet1RemoteF.map { unsignedCet1Remote =>
        ???
      }

      val unsignedCet2RemoteF = createCET2Remote(
        fundingTx = unsignedFundingSpendingInfo
      )

      val sig2F = unsignedCet2RemoteF.map { unsignedCet2Remote =>
        ???
      }

      // send sigs

      // validate their sigs
      val local1CETF: Future[Transaction] = remoteSig1F.map { remoteSig1 =>
        ???
      }

      val local2CETF: Future[Transaction] = remoteSig2F.map { remoteSig2 =>
        ???
      }

      // sign funding tx
      // publish funding tx

      // get oracle sig
      // publish corresponding CET
      // spend corresponding CET
      oracleSigF.map { oracleSig =>
        if (Schnorr.verify(message1, oracleSig, oraclePubKey)) {
          local1CETF.map { local1CET =>
            ???
          }
        } else if (Schnorr.verify(message2, oracleSig, oraclePubKey)) {
          local2CETF.map { local2CET =>
            ???
          }
        } else {
          ???
        }
      }
    }
  }
}
