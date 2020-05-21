package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  P2PKHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  ScriptSignatureParams,
  UnassignedSegwitNativeInputInfo
}
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPrivateKey, ECPublicKey}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class NonInteractiveWithChangeFinalizerTest extends BitcoinSAsyncTest {
  behavior of "NonInteractiveWithChangeFinalizerTest"

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
      NonInteractiveWithChangeFinalizer
        .sanityDestinationChecks(Vector(outPoint),
                                 Vector(output),
                                 changeSPK,
                                 missingOutputTx)
        .isFailure)
  }

  it should "detect extra outputs added" in {
    val newOutput = TransactionOutput(Bitcoins.max, EmptyScriptPubKey)
    val extraOutputTx = BaseTransaction(tx.version,
                                        tx.inputs,
                                        Vector(output, newOutput),
                                        tx.lockTime)

    assert(
      NonInteractiveWithChangeFinalizer
        .sanityDestinationChecks(Vector(outPoint),
                                 Vector(output),
                                 changeSPK,
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
      NonInteractiveWithChangeFinalizer
        .sanityDestinationChecks(Vector(outPoint),
                                 Vector(output),
                                 changeSPK,
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
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)

    recoverToSucceededIf[IllegalArgumentException] {
      NonInteractiveWithChangeFinalizer.txFrom(outputs = destinations,
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
      signer = privKey,
      hashType = HashType.sigHashAll
    )
    val utxos = Vector(utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(-1))

    recoverToSucceededIf[IllegalArgumentException] {
      NonInteractiveWithChangeFinalizer.txFrom(outputs = destinations,
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
        signers = Vector(privKey),
        hashType = HashType.sigHashAll
      )

    recoverToSucceededIf[UnsupportedOperationException] {
      NonInteractiveWithChangeFinalizer.txFrom(
        Vector(TransactionOutput(Bitcoins.one, EmptyScriptPubKey)),
        Vector(spendingInfo),
        SatoshisPerVirtualByte(Satoshis.one),
        EmptyScriptPubKey
      )
    }
  }
}
