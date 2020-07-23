package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  ScriptSignatureParams,
  UnassignedSegwitNativeInputInfo
}
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPrivateKey, ECPublicKey}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.{
  CreditingTxGen,
  CurrencyUnitGenerator,
  ScriptGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class ShufflingNonInteractiveFinalizerTest extends BitcoinSAsyncTest {
  behavior of "ShufflingNonInteractiveFinalizer"

  private val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  private val outPoint =
    TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero)

  private val input =
    TransactionInput(outPoint, EmptyScriptSignature, UInt32.zero)
  private val output = TransactionOutput(Bitcoins.one, EmptyScriptPubKey)

  private val tx = BaseTransaction(TransactionConstants.validLockVersion,
                                   Vector(input),
                                   Vector(output),
                                   UInt32.zero)

  private val changeSPK = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)

  it should "detect a missing destination" in {
    val missingOutputTx = BaseTransaction(tx.version,
                                          tx.inputs,
                                          Vector.empty[TransactionOutput],
                                          tx.lockTime)

    assert(
      SanityCheckFinalizer
        .sanityDestinationChecks(Vector(outPoint),
                                 Vector(EmptyScriptPubKey),
                                 Vector(changeSPK),
                                 missingOutputTx)
        .isFailure)
  }

  it should "detect extra outputs added" in {
    val newOutput =
      TransactionOutput(Bitcoins.one,
                        P2PKHScriptPubKey(ECPublicKey.freshPublicKey))
    val extraOutputTx = BaseTransaction(tx.version,
                                        tx.inputs,
                                        Vector(output, newOutput),
                                        tx.lockTime)

    assert(
      SanityCheckFinalizer
        .sanityDestinationChecks(Vector(outPoint),
                                 Vector(EmptyScriptPubKey),
                                 Vector(changeSPK),
                                 extraOutputTx)
        .isFailure)
  }

  it should "detect extra outpoints added" in {
    val newOutPoint =
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one)
    val newInput =
      TransactionInput(newOutPoint, EmptyScriptSignature, UInt32.zero)
    val extraOutPointTx = BaseTransaction(tx.version,
                                          Vector(input, newInput),
                                          tx.outputs,
                                          tx.lockTime)

    assert(
      SanityCheckFinalizer
        .sanityDestinationChecks(Vector(outPoint),
                                 Vector(EmptyScriptPubKey),
                                 Vector(changeSPK),
                                 extraOutPointTx)
        .isFailure)
  }

  it should "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = ScriptSignatureParams(
      InputInfo(
        outPoint = outPoint,
        output = creditingOutput,
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition,
        hashPreImages = Vector(privKey.publicKey)
      ),
      prevTransaction = creditingTx,
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)

    recoverToSucceededIf[IllegalArgumentException] {
      ShufflingNonInteractiveFinalizer.txFrom(outputs = destinations,
                                              utxos = utxos,
                                              feeRate = feeUnit,
                                              changeSPK = EmptyScriptPubKey)
    }
  }

  it should "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = ScriptSignatureParams(
      InputInfo(
        outPoint = outPoint,
        output = creditingOutput,
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition,
        hashPreImages = Vector(privKey.publicKey)
      ),
      prevTransaction = creditingTx,
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(-1))

    recoverToSucceededIf[IllegalArgumentException] {
      ShufflingNonInteractiveFinalizer.txFrom(outputs = destinations,
                                              utxos = utxos,
                                              feeRate = feeUnit,
                                              changeSPK = EmptyScriptPubKey)
    }
  }

  it should "fail to construct a tx given an UnassignedSegwitNativeUTXOSpendingInfo" in {
    val outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero)
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val spendingInfo =
      ScriptSignatureParams(
        UnassignedSegwitNativeInputInfo(
          outPoint = outPoint,
          amount = Bitcoins.one + CurrencyUnits.oneMBTC,
          scriptPubKey = P2WPKHWitnessSPKV0(pubKey),
          scriptWitness = P2WPKHWitnessV0(pubKey),
          conditionalPath = ConditionalPath.NoCondition,
          Vector(pubKey)
        ),
        prevTransaction = EmptyTransaction,
        signers = Vector(privKey),
        hashType = HashType.sigHashAll
      )

    recoverToSucceededIf[UnsupportedOperationException] {
      ShufflingNonInteractiveFinalizer.txFrom(
        Vector(TransactionOutput(Bitcoins.one, EmptyScriptPubKey)),
        Vector(spendingInfo),
        SatoshisPerVirtualByte(Satoshis.one),
        EmptyScriptPubKey
      )
    }
  }

  it must "create a shuffled transaction with a ShufflingNonInteractiveFinalizer" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(),
                CurrencyUnitGenerator.feeUnit(100),
                ScriptGenerators.scriptPubKey) {
      case ((inputs, outputs), feeRate, (changeSpk, _)) =>
        val txsF =
          FutureUtil.foldLeftAsync(Vector.empty[Transaction], 0 to 20) {
            (accum, _) =>
              ShufflingNonInteractiveFinalizer
                .txFrom(outputs, inputs, feeRate, changeSpk)
                .map(_ +: accum)
          }

        txsF.map { txs =>
          assert(
            inputs.size <= 1 || txs.exists(
              _.inputs.map(_.previousOutput) != inputs.map(_.outPoint)))
          assert(outputs.size <= 1 || txs.exists(_.outputs != outputs))
        }
    }
  }
}
