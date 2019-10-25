package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency._
import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.scalatest.{AsyncFlatSpec, MustMatchers}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.config.RegTest
import org.bitcoins.testkit.Implicits._

class BitcoinTxBuilderTest extends AsyncFlatSpec with MustMatchers {
  private val logger = BitcoinSLogger.logger
  val tc = TransactionConstants
  val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  behavior of "BitcoinTxBuilder"

  // We had a matcherror when passing in a vector of UTXOs,
  // because a match statement on a Seq relied on the ::
  // deconstructor. You would assume the compiler could
  // warn you about that...
  it must "work with a list and a vector of UTXOs" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                       output = creditingOutput,
                                       signers = Seq(privKey),
                                       redeemScriptOpt = None,
                                       scriptWitnessOpt = None,
                                       hashType = HashType.sigHashAll)

    val listF =
      BitcoinTxBuilder(destinations = Seq.empty,
                       utxos = List(utxo),
                       feeRate = SatoshisPerByte(1.sat),
                       changeSPK = EmptyScriptPubKey,
                       network = RegTest)

    val vecF =
      BitcoinTxBuilder(destinations = Seq.empty,
                       utxos = List(utxo),
                       feeRate = SatoshisPerByte(1.sat),
                       changeSPK = EmptyScriptPubKey,
                       network = RegTest)

    for {
      _ <- listF
      _ <- vecF
    } yield succeed

  }

  it must "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                       output = creditingOutput,
                                       signers = Seq(privKey),
                                       redeemScriptOpt = None,
                                       scriptWitnessOpt = None,
                                       hashType = HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    val resultFuture = txBuilder.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                       output = creditingOutput,
                                       signers = Seq(privKey),
                                       redeemScriptOpt = None,
                                       scriptWitnessOpt = None,
                                       hashType = HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(Int64(-1)))
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    recoverToSucceededIf[IllegalArgumentException] {
      txBuilder
    }
  }

  it must "fail a transaction when the user invariants fail" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint,
                                       creditingOutput,
                                       Seq(privKey),
                                       None,
                                       None,
                                       HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(currencyUnit = Satoshis(Int64(1)))
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    //trivially false
    val f = (_: Seq[BitcoinUTXOSpendingInfo], _: Transaction) => false
    val resultFuture = txBuilder.flatMap(_.sign(f))
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "be able to create a BitcoinTxBuilder from UTXOTuple and UTXOMap" in {
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = spk)
    val destinations = {
      Seq(
        TransactionOutput(value = Satoshis.one,
                          scriptPubKey = EmptyScriptPubKey))
    }
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                       output = creditingOutput,
                                       signers = Seq(privKey),
                                       redeemScriptOpt = None,
                                       scriptWitnessOpt = None,
                                       hashType = HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)
    val utxoSpendingInfo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                                   output = creditingOutput,
                                                   signers = Seq(privKey),
                                                   redeemScriptOpt = None,
                                                   scriptWitnessOpt = None,
                                                   hashType =
                                                     HashType.sigHashAll)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderMap = BitcoinTxBuilder(destinations = destinations,
                                        utxos = utxoMap,
                                        feeRate = feeUnit,
                                        changeSPK = EmptyScriptPubKey,
                                        network = TestNet3)
    val txBuilderTuple = BitcoinTxBuilder(destinations = destinations,
                                          utxos = Seq(utxoSpendingInfo),
                                          feeRate = feeUnit,
                                          changeSPK = EmptyScriptPubKey,
                                          network = TestNet3)

    txBuilderTuple.flatMap { tup =>
      txBuilderMap.map { map =>
        assert(map == tup)
      }
    }
  }

  it must "fail to build a tx if you have the wrong redeemscript" in {
    val p2sh = P2SHScriptPubKey(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = creditingOutput,
        signers = Seq(privKey),
        redeemScriptOpt = Some(EmptyScriptPubKey),
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll
      )
    }
  }

  it must "fail to build a tx if you have the wrong script witness" in {
    val p2wsh = P2WSHWitnessSPKV0(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wsh)
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(outPoint,
                              creditingOutput,
                              Seq(privKey),
                              None,
                              Some(P2WSHWitnessV0(EmptyScriptPubKey)),
                              HashType.sigHashAll)
    }
  }

  it must "fail to sign a p2pkh if we don't pass in the public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint,
                                       creditingOutput,
                                       Seq(privKey),
                                       None,
                                       Some(P2WSHWitnessV0(EmptyScriptPubKey)),
                                       HashType.sigHashAll)
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderWitness = BitcoinTxBuilder(destinations,
                                            utxoMap,
                                            feeUnit,
                                            EmptyScriptPubKey,
                                            TestNet3)
    val resultFuture = txBuilderWitness.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to sign a p2pkh if we pass in the wrong public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val pubKey2 = ECPrivateKey().publicKey
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
      hashType = HashType.sigHashAll
    )
    val utxoMap: BitcoinTxBuilder.UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderWitness = BitcoinTxBuilder(destinations = destinations,
                                            utxos = utxoMap,
                                            feeRate = feeUnit,
                                            changeSPK = EmptyScriptPubKey,
                                            network = TestNet3)
    val resultFuture = txBuilderWitness.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to sign a p2wpkh if we don't pass in the public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(pubKey = privKey.publicKey)
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = p2wpkh)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = creditingOutput,
        signers = Seq(privKey),
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        hashType = HashType.sigHashAll
      )
    }
  }

  it must "fail to sign a p2wpkh if we pass in the wrong public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(outPoint,
                              creditingOutput,
                              Seq(privKey),
                              None,
                              Some(P2WSHWitnessV0(EmptyScriptPubKey)),
                              HashType.sigHashAll)
    }
  }
}
