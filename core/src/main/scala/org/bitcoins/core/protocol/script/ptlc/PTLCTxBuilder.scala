package org.bitcoins.core.protocol.script.ptlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSignerSingle
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
  ConditionalPath,
  P2WSHV0SpendingInfoSingle
}

import scala.concurrent.{ExecutionContext, Future}

/** 222 (witness) + 22 (p2wpkh output) */
case class PTLCTxBuilder(
    paymentAmt: CurrencyUnit,
    payerFundingKey: ECPublicKey,
    receiverFundingKey: ECPublicKey,
    fundingUtxosOpt: Option[Vector[BitcoinUTXOSpendingInfoFull]],
    unsignedFundingTxOpt: Option[Transaction],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork,
    spendingVBytes: Long = 244)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {
  require(fundingUtxosOpt.isEmpty != unsignedFundingTxOpt.isEmpty,
          "One of fundingUtxosOpt or unsignedFundingTxOpt must be defined")

  val fundingSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(2, Vector(payerFundingKey, receiverFundingKey))
  }

  lazy val (unsignedFundingTransaction, signedFundingTransaction) = {
    (fundingUtxosOpt, unsignedFundingTxOpt) match {
      case (Some(fundingUtxos), _) =>
        val txBuilderF: Future[BitcoinTxBuilder] = {
          val spendingFee = spendingVBytes * feeRate.toLong

          val output: TransactionOutput =
            TransactionOutput(paymentAmt + Satoshis(spendingFee),
                              P2WSHWitnessSPKV0(fundingSPK))

          BitcoinTxBuilder(Vector(output),
                           fundingUtxos,
                           feeRate,
                           changeSPK,
                           network)
        }

        val unsignedFundingTransaction: Future[Transaction] = {
          txBuilderF.flatMap(_.unsignedTx)
        }

        val signedFundingTransaction: Future[Transaction] = {
          txBuilderF.flatMap(_.sign)
        }

        (unsignedFundingTransaction, signedFundingTransaction)
      case (None, Some(unsignedFundingTx)) =>
        // This party does not get or need the signed funding tx
        (Future.successful(unsignedFundingTx),
         Future.successful(unsignedFundingTx))
      case (None, None) =>
        throw new IllegalArgumentException("This can't happen")
    }
  }

  private def createSpendingTxAndSpendingInfo(
      fundingTx: Transaction,
      address: BitcoinAddress,
      fundingPrivKey: ECPrivateKey,
      nLockTime: UInt32 = UInt32.zero): (
      BaseTransaction,
      P2WSHV0SpendingInfoSingle) = {
    val outPoint = TransactionOutPoint(fundingTx.txIdBE, UInt32.zero)

    val tx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(outPoint,
                         EmptyScriptSignature,
                         TransactionConstants.sequence)),
      Vector(TransactionOutput(paymentAmt, address.scriptPubKey)),
      nLockTime
    )

    val spendingInfo = P2WSHV0SpendingInfoSingle(
      outPoint,
      fundingTx.outputs.head.value,
      P2WSHWitnessSPKV0(fundingSPK),
      fundingPrivKey,
      HashType.sigHashAll,
      P2WSHWitnessV0(fundingSPK),
      ConditionalPath.NoConditionsLeft
    )

    (tx, spendingInfo)
  }

  private def createSignedSpendingTx(
      remoteSig: PartialSignature,
      address: BitcoinAddress,
      fundingPrivKey: ECPrivateKey,
      timeout: UInt32 = UInt32.zero): Future[Transaction] = {
    unsignedFundingTransaction.flatMap { fundingTx =>
      val (tx, _) =
        createSpendingTxAndSpendingInfo(fundingTx,
                                        address,
                                        fundingPrivKey,
                                        timeout)

      PSBT
        .fromUnsignedTx(tx)
        .addWitnessUTXOToInput(fundingTx.outputs.head, 0)
        .addScriptWitnessToInput(P2WSHWitnessV0(fundingSPK), 0)
        .addSignatures(Vector(remoteSig), 0)
        .sign(0, fundingPrivKey) // TODO This sometimes needs to be adapted
        .flatMap { signedSpendingPSBT =>
          val txT =
            signedSpendingPSBT.finalizePSBT.flatMap(
              _.extractTransactionAndValidate)
          Future.fromTry(txT)
        }
    }
  }

  def createAdaptorSig(
      remoteFinalAddress: BitcoinAddress,
      fundingPrivKey: ECPrivateKey): Future[PartialSignature] = {
    unsignedFundingTransaction.flatMap { fundingTx =>
      val (tx, spendingInfo) =
        createSpendingTxAndSpendingInfo(fundingTx,
                                        remoteFinalAddress,
                                        fundingPrivKey)

      // TODO: This needs to be adapted
      BitcoinSignerSingle.signSingle(spendingInfo, tx, isDummySignature = false)
    }
  }

  def createNormalSpendingTx(
      remoteSig: PartialSignature,
      finalAddress: BitcoinAddress,
      fundingPrivKey: ECPrivateKey): Future[Transaction] = {
    createSignedSpendingTx(remoteSig, finalAddress, fundingPrivKey)
  }

  def createRefundSig(
      refundAddress: BitcoinAddress,
      fundingPrivKey: ECPrivateKey,
      timeout: UInt32): Future[PartialSignature] = {
    unsignedFundingTransaction.flatMap { fundingTx =>
      val (tx, spendingInfo) = createSpendingTxAndSpendingInfo(fundingTx,
                                                               refundAddress,
                                                               fundingPrivKey,
                                                               timeout)

      BitcoinSignerSingle.signSingle(spendingInfo, tx, isDummySignature = false)
    }
  }

  def createRefundTx(
      remoteSig: PartialSignature,
      refundAddress: BitcoinAddress,
      fundingPrivKey: ECPrivateKey,
      timeout: UInt32): Future[Transaction] = {
    createSignedSpendingTx(remoteSig, refundAddress, fundingPrivKey, timeout)
  }
}
