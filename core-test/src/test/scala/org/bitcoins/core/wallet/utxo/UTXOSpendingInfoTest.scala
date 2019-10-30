package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionConstants,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.testkit.core.gen.{ScriptGenerators, TransactionGenerators}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class UTXOSpendingInfoTest extends BitcoinSAsyncTest {

  def randomSPK: ScriptPubKey = {
    ScriptGenerators.scriptPubKey.map(_._1).sample.get
  }

  def randomNonWitnessSPK: NonWitnessScriptPubKey = {
    ScriptGenerators.nonWitnessScriptPubKey.map(_._1).sample.get
  }

  def randomWitnessSPK: WitnessScriptPubKeyV0 = {
    ScriptGenerators.witnessScriptPubKeyV0.map(_._1).sample.get
  }

  behavior of "UTXOSpendingInfo"

  it must "fail given a bad P2SH script" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey
    val p2sh = P2SHScriptPubKey(P2PKScriptPubKey(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNoNestSpendingInfo(outPoint = outPoint,
                             amount = CurrencyUnits.zero,
                             scriptPubKey = p2sh,
                             signers = Seq(privKey),
                             hashType = HashType.sigHashAll,
                             redeemScript = randomNonWitnessSPK)
    }
  }

  it must "fail given a bad P2SH-P2WPKH redeem script" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WPKHWitnessSPKV0(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNestedSegwitV0UTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = randomWitnessSPK,
        scriptWitness = P2WPKHWitnessV0(pubKey)
      )
    }
  }

  it must "fail given a bad P2SH-P2WPKH script witness" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WPKHWitnessSPKV0(pubKey))
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version =
                                        TransactionConstants.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNestedSegwitV0UTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = P2WPKHWitnessSPKV0(pubKey),
        scriptWitness = P2WPKHWitnessV0(ECPrivateKey.freshPrivateKey.publicKey)
      )
    }
  }

  it must "fail given a bad P2SH-P2WSH redeem script" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WSHWitnessSPKV0(P2PKScriptPubKey(pubKey)))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNestedSegwitV0UTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = randomWitnessSPK,
        scriptWitness = P2WSHWitnessV0(P2PKScriptPubKey(pubKey))
      )
    }
  }

  it must "fail given a bad P2SH-P2WSH script witness" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WSHWitnessSPKV0(P2PKScriptPubKey(pubKey)))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNestedSegwitV0UTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = P2WSHWitnessSPKV0(P2PKScriptPubKey(pubKey)),
        scriptWitness = P2WSHWitnessV0(randomSPK)
      )
    }
  }

  it must "fail given a bad P2WPKH script witness" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2wpkh = P2WPKHWitnessSPKV0(pubKey)
    val (creditingTx, _) =
      TransactionGenerators.buildCreditingTransaction(p2wpkh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      SegwitV0NativeUTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2wpkh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(ECPrivateKey.freshPrivateKey.publicKey)
      )
    }
  }

  it must "fail given a bad P2WSH script witness" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2wsh = P2WSHWitnessSPKV0(P2PKScriptPubKey(pubKey))
    val (creditingTx, _) =
      TransactionGenerators.buildCreditingTransaction(p2wsh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      SegwitV0NativeUTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2wsh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = P2WSHWitnessV0(randomSPK)
      )
    }
  }
}
