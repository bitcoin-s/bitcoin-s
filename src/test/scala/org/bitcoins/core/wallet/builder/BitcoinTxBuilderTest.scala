package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.{CryptoGenerators, ScriptGenerators}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.scalatest.{FlatSpec, MustMatchers}

class BitcoinTxBuilderTest extends FlatSpec with MustMatchers {
  private val logger = BitcoinSLogger.logger
  val tc = TransactionConstants
  val (spk,privKey) = ScriptGenerators.p2pkhScriptPubKey.sample.get
  "TxBuilder" must "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)),EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,Nil,Seq(creditingOutput),tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxoMap: TxBuilder.UTXOMap = Map(outPoint -> (creditingOutput,Seq(signer),None,None,HashType.sigHashAll))
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations,Seq(creditingTx),utxoMap,feeUnit,EmptyScriptPubKey, TestNet3)
    val result = txBuilder.left.flatMap(_.sign)
    result must be (Right(TxBuilderError.MintsMoney))
  }

  it must "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)),EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,Nil,Seq(creditingOutput),tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxoMap: TxBuilder.UTXOMap = Map(outPoint -> (creditingOutput,Seq(signer),None,None,HashType.sigHashAll))
    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(-1)))
    val txBuilder = BitcoinTxBuilder(destinations,Seq(creditingTx),utxoMap,feeUnit,EmptyScriptPubKey, TestNet3)
    txBuilder must be (Right(TxBuilderError.LowFee))
  }

  it must "fail to build a txbuilder when we do not pass in all the crediting txs whose outpoints are specified in utxo map" in {
    val creditingOutput = TransactionOutput(Satoshis(Int64(1)), spk)
    val destinations = Seq(TransactionOutput(CurrencyUnits.zero,EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,Nil,Seq(creditingOutput),tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val outPoint2 = TransactionOutPoint(CryptoGenerators.doubleSha256Digest.sample.get,UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxoMap: TxBuilder.UTXOMap = Map(outPoint -> (creditingOutput,Seq(signer),None,None,HashType.sigHashAll),
      outPoint2 -> (creditingOutput,Seq(signer),None,None,HashType.sigHashAll)
    )
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations,Seq(creditingTx),utxoMap,feeUnit,EmptyScriptPubKey, TestNet3)
    txBuilder must be (Right(TxBuilderError.MissingCreditingTx))
  }
}
