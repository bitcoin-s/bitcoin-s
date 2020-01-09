package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey}
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
import org.bitcoins.testkit.core.gen.{
  GenUtil,
  ScriptGenerators,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class UTXOSpendingInfoTest extends BitcoinSAsyncTest {

  def randomSPK: ScriptPubKey = {
    GenUtil.sample(ScriptGenerators.scriptPubKey.map(_._1))
  }

  def randomRawSPK: RawScriptPubKey = {
    GenUtil.sample(ScriptGenerators.rawScriptPubKey.map(_._1))
  }

  def randomWitnessSPK: WitnessScriptPubKeyV0 = {
    GenUtil.sample(ScriptGenerators.witnessScriptPubKeyV0.map(_._1))
  }

  behavior of "UTXOSpendingInfo"

  it must "fail given a bad P2SH script" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey
    val p2sh = P2SHScriptPubKey(P2PKScriptPubKey(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNoNestSpendingInfoFull(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = randomRawSPK,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail given a bad P2SH-P2WPKH redeem script" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2sh = P2SHScriptPubKey(P2WPKHWitnessSPKV0(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      P2SHNestedSegwitV0UTXOSpendingInfoFull(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = randomWitnessSPK,
        scriptWitness = P2WPKHWitnessV0(pubKey),
        conditionalPath = ConditionalPath.NoConditionsLeft
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
      P2SHNestedSegwitV0UTXOSpendingInfoFull(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = P2WPKHWitnessSPKV0(pubKey),
        scriptWitness = P2WPKHWitnessV0(ECPrivateKey.freshPrivateKey.publicKey),
        conditionalPath = ConditionalPath.NoConditionsLeft
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
      P2SHNestedSegwitV0UTXOSpendingInfoFull(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = randomWitnessSPK,
        scriptWitness = P2WSHWitnessV0(P2PKScriptPubKey(pubKey)),
        conditionalPath = ConditionalPath.NoConditionsLeft
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
      P2SHNestedSegwitV0UTXOSpendingInfoFull(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = p2sh,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScript = P2WSHWitnessSPKV0(P2PKScriptPubKey(pubKey)),
        scriptWitness = P2WSHWitnessV0(randomRawSPK),
        conditionalPath = ConditionalPath.NoConditionsLeft
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
        scriptWitness = P2WPKHWitnessV0(ECPrivateKey.freshPrivateKey.publicKey),
        conditionalPath = ConditionalPath.NoConditionsLeft
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
        scriptWitness = P2WSHWitnessV0(randomRawSPK),
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail validation given invalid witnesses" in {
    val p2wpkh = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
    val p2wsh = P2WSHWitnessSPKV0(randomRawSPK)
    val p2wshWitness = P2WSHWitnessV0(randomRawSPK)
    val p2WPKHWitness = P2WPKHWitnessV0(ECPublicKey.freshPublicKey)

    assert(!BitcoinUTXOSpendingInfo.isValidScriptWitness(p2wpkh, p2wshWitness))
    assert(!BitcoinUTXOSpendingInfo.isValidScriptWitness(p2wsh, p2WPKHWitness))
  }

  it must "fail given no redeem script on P2SH" in {
    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey
    val p2sh = P2SHScriptPubKey(P2PKScriptPubKey(pubKey))
    val (creditingTx, _) = TransactionGenerators.buildCreditingTransaction(p2sh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoConditionsLeft
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
                                      outputs = Seq(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = Some(P2WPKHWitnessSPKV0(pubKey)),
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoConditionsLeft
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
                                      outputs = Seq(creditingOutput),
                                      lockTime = TransactionConstants.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    assertThrows[UnsupportedOperationException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = Some(P2WPKHWitnessSPKV0(pubKey)),
        scriptWitnessOpt = Some(EmptyScriptWitness),
        conditionalPath = ConditionalPath.NoConditionsLeft
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

    assertThrows[UnsupportedOperationException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2sh),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = Some(unassingedWitnessSPK),
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoConditionsLeft
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
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2wpkh),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(EmptyScriptWitness),
        conditionalPath = ConditionalPath.NoConditionsLeft
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
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, p2wpkh),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it should "successfully return UnassingedSegwitNativeUTXOSpendingInfo" in {
    val unassingedWitnessSPK = UnassignedWitnessScriptPubKey.fromAsm(
      P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey).asm)

    val privKey = ECPrivateKey.freshPrivateKey
    val pubKey = privKey.publicKey

    val p2wpkh = P2WPKHWitnessSPKV0(pubKey)
    val (creditingTx, _) =
      TransactionGenerators.buildCreditingTransaction(p2wpkh)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    val spendingInfo =
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, unassingedWitnessSPK),
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )

    val expectedSpendingInfo =
      UnassignedSegwitNativeUTXOSpendingInfo(
        outPoint = outPoint,
        amount = CurrencyUnits.zero,
        scriptPubKey = unassingedWitnessSPK,
        signers = Seq(privKey),
        hashType = HashType.sigHashAll,
        scriptWitness = EmptyScriptWitness,
        conditionalPath = ConditionalPath.NoConditionsLeft
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
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = TransactionOutput(CurrencyUnits.zero, spk),
        signers = Seq(privKey),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }
}
