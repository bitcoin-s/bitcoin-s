package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionConstants,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest

class InputSigningInfoTest extends BitcoinSUnitTest {
  behavior of "InputSigningInfo"

  private val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  it should "fail to build a tx if you have the wrong redeemscript" in {
    val p2sh = P2SHScriptPubKey(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val inputInfo = InputInfo(
      outPoint = outPoint,
      output = creditingOutput,
      redeemScriptOpt = Some(EmptyScriptPubKey),
      scriptWitnessOpt = None,
      conditionalPath = ConditionalPath.NoCondition
    )

    assertThrows[RuntimeException] {
      ScriptSignatureParams(
        inputInfo = inputInfo,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    }

    assertThrows[RuntimeException] {
      ECSignatureParams(
        inputInfo = inputInfo,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    }
  }

  it should "fail to build a tx if you have the wrong script witness" in {
    val p2wsh = P2WSHWitnessSPKV0(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wsh)
    val creditingTx = BaseTransaction(TransactionConstants.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      ScriptSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        privKey,
        HashType.sigHashAll
      )
    }

    assertThrows[IllegalArgumentException] {
      ECSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        privKey,
        HashType.sigHashAll
      )
    }
  }

  it should "fail to sign a p2wpkh if we don't pass in the public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(pubKey = privKey.publicKey)
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = p2wpkh)
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Vector(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    val inputInfo = InputInfo(
      outPoint = outPoint,
      output = creditingOutput,
      redeemScriptOpt = None,
      scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
      conditionalPath = ConditionalPath.NoCondition
    )

    assertThrows[IllegalArgumentException] {
      ScriptSignatureParams(
        inputInfo = inputInfo,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    }

    assertThrows[IllegalArgumentException] {
      ECSignatureParams(
        inputInfo = inputInfo,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    }
  }

  it should "fail to sign a p2wpkh if we pass in the wrong public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val creditingTx = BaseTransaction(TransactionConstants.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      ScriptSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        privKey,
        HashType.sigHashAll
      )
    }

    assertThrows[IllegalArgumentException] {
      ECSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        privKey,
        HashType.sigHashAll
      )
    }
  }
}
