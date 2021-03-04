package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class InputSigningInfoTest extends BitcoinSUnitTest {
  behavior of "InputSigningInfo"

  private val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  it should "fail to build a tx if you have the wrong redeem script" in {
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
        prevTransaction = creditingTx,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    }

    assertThrows[RuntimeException] {
      ECSignatureParams(
        inputInfo = inputInfo,
        prevTransaction = creditingTx,
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
        prevTransaction = creditingTx,
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
        prevTransaction = creditingTx,
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
        prevTransaction = creditingTx,
        signer = privKey,
        hashType = HashType.sigHashAll
      )
    }

    assertThrows[IllegalArgumentException] {
      ECSignatureParams(
        inputInfo = inputInfo,
        prevTransaction = creditingTx,
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
        prevTransaction = creditingTx,
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
        prevTransaction = creditingTx,
        privKey,
        HashType.sigHashAll
      )
    }
  }

  it should "fail to sign if the prevTransaction does not match the outPoint" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val creditingTx = BaseTransaction(TransactionConstants.validLockVersion,
                                      Nil,
                                      Vector(creditingOutput),
                                      TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      ScriptSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = Some(P2WPKHWitnessV0(privKey.publicKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        prevTransaction = creditingTx,
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
          scriptWitnessOpt = Some(P2WPKHWitnessV0(privKey.publicKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        prevTransaction = creditingTx,
        privKey,
        HashType.sigHashAll
      )
    }
  }

  it should "fail to sign if the prevTransaction's output does not match the amount" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val creditingTx =
      BaseTransaction(TransactionConstants.validLockVersion,
                      Nil,
                      Vector(TransactionOutput(CurrencyUnits.oneBTC, p2wpkh)),
                      TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      ScriptSignatureParams(
        InputInfo(
          outPoint = outPoint,
          output = creditingOutput,
          redeemScriptOpt = None,
          scriptWitnessOpt = Some(P2WPKHWitnessV0(privKey.publicKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        prevTransaction = creditingTx,
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
          scriptWitnessOpt = Some(P2WPKHWitnessV0(privKey.publicKey)),
          conditionalPath = ConditionalPath.NoCondition,
          hashPreImages = Vector(privKey.publicKey)
        ),
        prevTransaction = creditingTx,
        privKey,
        HashType.sigHashAll
      )
    }
  }
}
