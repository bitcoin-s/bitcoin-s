package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.{ CurrencyUnits, Satoshis }
import org.bitcoins.core.gen.{ CryptoGenerators, ScriptGenerators }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder.UTXOMap
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.scalatest.{ FlatSpec, MustMatchers }

class BitcoinTxBuilderTest extends FlatSpec with MustMatchers {
  private val logger = BitcoinSLogger.logger
  val tc = TransactionConstants
  val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sample.get
  "TxBuilder" must "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None, None, HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilder.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.MintsMoney))
  }

  it must "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None, None, HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(-1)))
    val txBuilder = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    txBuilder must be(Right(TxBuilderError.LowFee))
  }

  it must "fail to build a txbuilder when we do not pass in all the crediting txs whose outpoints are specified in utxo map" in {
    val creditingOutput = TransactionOutput(Satoshis(Int64(1)), spk)
    val destinations = Seq(TransactionOutput(CurrencyUnits.zero, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val outPoint2 = TransactionOutPoint(CryptoGenerators.doubleSha256Digest.sample.get, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(
      outPoint -> BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None, None, HashType.sigHashAll),
      outPoint2 -> BitcoinUTXOSpendingInfo(outPoint2, creditingOutput, Seq(signer), None, None, HashType.sigHashAll))
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    txBuilder must be(Right(TxBuilderError.MissingCreditingTx))
  }

  it must "fail a transaction when the user invariants fail" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None, None, HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilder = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    //trivially false
    val f = (_: Seq[BitcoinUTXOSpendingInfo], _: Transaction) => false
    val result = txBuilder.left.flatMap(_.sign(f))
    result must be(Right(TxBuilderError.FailedUserInvariants))
  }

  it must "be able to create a BitcoinTxBuilder from UTXOTuple and UTXOMap" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxo = wallet.utxo.BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None, None, HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val uTXOSpendingInfo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None, None, HashType.sigHashAll)

    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderMap = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val txBuilderTuple = BitcoinTxBuilder(destinations, Seq(creditingTx), Seq(uTXOSpendingInfo), feeUnit, EmptyScriptPubKey, TestNet3)

    txBuilderTuple must be(txBuilderMap)
  }

  it must "fail to build a tx if you have the wrong redeemscript" in {
    val p2sh = P2SHScriptPubKey(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), Some(EmptyScriptPubKey), None, HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderNoRedeem = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilderNoRedeem.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.WrongRedeemScript))
  }

  it must "fail to build a tx if you have the wrong script witness" in {
    val p2wsh = P2WSHWitnessSPKV0(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wsh)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(privKey.publicKey))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)), HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderWitness = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilderWitness.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.WrongWitness))
  }

  it must "fail to sign a p2pkh if we don't pass in the public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), None)
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)), HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderWitness = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilderWitness.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.MissingPublicKey))
  }

  it must "fail to sign a p2pkh if we pass in the wrong public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val pubKey2 = ECPrivateKey().publicKey
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(pubKey2))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)), HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderWitness = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilderWitness.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.WrongPublicKey))
  }

  it must "fail to sign a p2wpkh if we don't pass in the public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), None)
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)), HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderWitness = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilderWitness.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.MissingPublicKey))
  }

  it must "fail to sign a p2wpkh if we pass in the wrong public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val pubKey2 = ECPrivateKey().publicKey
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val destinations = Seq(TransactionOutput(Satoshis(Int64(1)), EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion, Nil, Seq(creditingOutput), tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val signer = (privKey.sign(_: Seq[Byte]), Some(pubKey2))
    val utxo = BitcoinUTXOSpendingInfo(outPoint, creditingOutput, Seq(signer), None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)), HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(1)))
    val txBuilderWitness = BitcoinTxBuilder(destinations, Seq(creditingTx), utxoMap, feeUnit, EmptyScriptPubKey, TestNet3)
    val result = txBuilderWitness.left.flatMap(_.sign)
    result must be(Right(TxBuilderError.WrongPublicKey))
  }
}
