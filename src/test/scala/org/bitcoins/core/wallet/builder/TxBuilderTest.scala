package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.{CryptoGenerators, ScriptGenerators}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.scalatest.{FlatSpec, MustMatchers}

class TxBuilderTest extends FlatSpec with MustMatchers {
  val tc = TransactionConstants
  val (spk,privKey) = ScriptGenerators.p2pkhScriptPubKey.sample.get
  "TxBuilder" must "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)),EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,Nil,Seq(creditingOutput),tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxoMap: TxBuilder.UTXOMap = Map(outPoint -> (creditingOutput,Seq(privKey),None,None,HashType.sigHashAll))
    val txBuilder = TxBuilder(destinations,Seq(creditingTx),utxoMap,1,EmptyScriptPubKey)
    val result = txBuilder.left.flatMap(_.sign((_,_) => true))
    result must be (Right(TxBuilderError.MintsMoney))
  }

  it must "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)),EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,Nil,Seq(creditingOutput),tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxoMap: TxBuilder.UTXOMap = Map(outPoint -> (creditingOutput,Seq(privKey),None,None,HashType.sigHashAll))
    val txBuilder = TxBuilder(destinations,Seq(creditingTx),utxoMap,-1,EmptyScriptPubKey)
    txBuilder must be (Right(TxBuilderError.BadFee))
  }

  it must "fail to build a txbuilder when we do not pass in all the crediting txs whose outpoints are specified in utxo map" in {
    val creditingOutput = TransactionOutput(Satoshis(Int64(1)), spk)
    val destinations = Seq(TransactionOutput(CurrencyUnits.zero,EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,Nil,Seq(creditingOutput),tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val outPoint2 = TransactionOutPoint(CryptoGenerators.doubleSha256Digest.sample.get,UInt32.zero)
    val utxoMap: TxBuilder.UTXOMap = Map(outPoint -> (creditingOutput,Seq(privKey),None,None,HashType.sigHashAll),
      outPoint2 -> (creditingOutput,Seq(privKey),None,None,HashType.sigHashAll)
    )
    val txBuilder = TxBuilder(destinations,Seq(creditingTx),utxoMap,1,EmptyScriptPubKey)
    txBuilder must be (Right(TxBuilderError.MissingCreditingTx))
  }
}
