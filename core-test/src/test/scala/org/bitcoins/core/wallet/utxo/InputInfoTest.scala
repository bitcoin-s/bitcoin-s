package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.testkit.core.gen.{
  CreditingTxGen,
  GenUtil,
  ScriptGenerators,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class InputInfoTest extends BitcoinSAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  def randomSPK: ScriptPubKey = {
    GenUtil.sample(ScriptGenerators.scriptPubKey.map(_._1))
  }

  def randomRawSPK: RawScriptPubKey = {
    GenUtil.sample(ScriptGenerators.rawScriptPubKey.map(_._1))
  }

  def randomWitnessSPK: WitnessScriptPubKeyV0 = {
    GenUtil.sample(ScriptGenerators.witnessScriptPubKeyV0.map(_._1))
  }

  behavior of "InputInfo"

  it must "fail given no redeem script on P2SH" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey
    val p2sh = P2SHScriptPubKey(P2PKScriptPubKey(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it should "fail given no script witness on P2SH-Segwit" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WPKHWitnessSPKV0(pubKey))
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        redeemScriptOpt = Some(P2WPKHWitnessSPKV0(pubKey)),
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it should "fail given an unsupported script witness on P2SH-Segwit" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WPKHWitnessSPKV0(pubKey))
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[UnsupportedOperationException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        redeemScriptOpt = Some(P2WPKHWitnessSPKV0(pubKey)),
        scriptWitnessOpt = Some(EmptyScriptWitness),
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it should "fail given UnassignedWitnessScriptPubKey redeemScript" in {
    val unassingedWitnessSPK = UnassignedWitnessScriptPubKey.fromAsm(
      P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey).asm)

    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey
    val p2sh = P2SHScriptPubKey(P2PKScriptPubKey(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[RuntimeException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        redeemScriptOpt = Some(unassingedWitnessSPK),
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it should "fail given unsupported witness" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2wpkh = P2WPKHWitnessSPKV0(pubKey)
    val (creditingTx, _) =
      TransactionGenerators.buildCreditingTransaction(p2wpkh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[UnsupportedOperationException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2wpkh),
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(EmptyScriptWitness),
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it should "fail given no witness" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2wpkh = P2WPKHWitnessSPKV0(pubKey)
    val (creditingTx, _) =
      TransactionGenerators.buildCreditingTransaction(p2wpkh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2wpkh),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it should "successfully return UnassignedSegwitNativeUTXOSpendingInfoFull" in {
    val unassingedWitnessSPK = UnassignedWitnessScriptPubKey.fromAsm(
      P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey).asm)

    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2wpkh = P2WPKHWitnessSPKV0(pubKey)
    val (creditingTx, _) =
      TransactionGenerators.buildCreditingTransaction(p2wpkh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    val spendingInfo =
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, unassingedWitnessSPK),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition
      )

    val expectedSpendingInfo =
      UnassignedSegwitNativeInputInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = unassingedWitnessSPK,
        scriptWitness = EmptyScriptWitness,
        conditionalPath = ConditionalPath.NoCondition,
        pubKeys = Vector.empty
      )

    assert(spendingInfo == expectedSpendingInfo)
  }

  it should "fail given a NonStandardScriptPubKey" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey
    val p2pk = P2PKScriptPubKey(pubKey)
    val spk = NonStandardScriptPubKey.fromAsm(p2pk.asm)
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2pk)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[UnsupportedOperationException] {
      InputInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, spk),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoCondition
      )
    }
  }

  it must "successfully compute maxWitnessLengths" in {
    forAllAsync(CreditingTxGen.output) { scriptSigParams =>
      val dummyTx = BaseTransaction(
        TransactionConstants.validLockVersion,
        Vector(
          TransactionInput(scriptSigParams.inputInfo.outPoint,
                           EmptyScriptSignature,
                           UInt32.zero)),
        Vector(TransactionOutput(Satoshis.zero, EmptyScriptPubKey)),
        UInt32.zero
      )

      val maxWitnessLenF = BitcoinSigner
        .sign(scriptSigParams, unsignedTx = dummyTx, isDummySignature = true)
        .map(_.transaction)
        .map {
          case wtx: WitnessTransaction  => wtx.witness.head.byteSize.toInt
          case _: NonWitnessTransaction => 0
        }

      maxWitnessLenF.map { expectedLen =>
        assert(scriptSigParams.maxWitnessLen == expectedLen)
      }
    }
  }

  it must "successfully compute maxScriptSigLengths" in {
    forAllAsync(CreditingTxGen.output) { scriptSigParams =>
      val dummyTx = BaseTransaction(
        TransactionConstants.validLockVersion,
        Vector(
          TransactionInput(scriptSigParams.inputInfo.outPoint,
                           EmptyScriptSignature,
                           UInt32.zero)),
        Vector(TransactionOutput(Satoshis.zero, EmptyScriptPubKey)),
        UInt32.zero
      )

      val maxScriptSigF = BitcoinSigner
        .sign(scriptSigParams, unsignedTx = dummyTx, isDummySignature = true)
        .map(_.transaction)
        .map { tx =>
          tx.inputs.head.scriptSignature
        }

      maxScriptSigF.map { scriptSig =>
        assert(InputInfo.maxScriptSigLen(
                 scriptSigParams.inputInfo) == scriptSig.byteSize,
               scriptSig.hex)
      }
    }
  }
}
