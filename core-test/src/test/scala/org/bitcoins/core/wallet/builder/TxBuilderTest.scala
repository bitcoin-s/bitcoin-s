package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  NonStandardScriptPubKey,
  P2PKHScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.EmptyInputInfo
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPublicKey}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TxBuilderTest extends BitcoinSUnitTest {

  "TxBuilder" must "detect a bad fee on the tx" in {
    val estimatedFee = 1000.sats
    val actualFee = 1.sat
    val feeRate = SatoshisPerVirtualByte(1.sat)
    Transaction
      .isValidFeeRange(estimatedFee, actualFee, feeRate)
      .isFailure must be(true)

    Transaction
      .isValidFeeRange(actualFee, estimatedFee, feeRate)
      .isFailure must be(true)
  }

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

  private val inputInfos = Vector(
    EmptyInputInfo(outPoint, Bitcoins.one + Bitcoins.one))
  private val feeRate = SatoshisPerVirtualByte(Satoshis.one)

  it must "detect a missing destination" in {
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

  it must "detect extra outputs added" in {
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

  it must "detect extra outpoints added" in {
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

  it must "detect impossibly high fees" in {
    val newOutput = TransactionOutput(Bitcoins.zero, EmptyScriptPubKey)
    val highFeeTx =
      BaseTransaction(tx.version, tx.inputs, Vector(newOutput), tx.lockTime)

    assert(
      Transaction
        .sanityAmountChecks(forSigned = true, inputInfos, feeRate, highFeeTx)
        .isFailure)
  }

  it must "detect dust outputs" in {
    val newOutput = TransactionOutput(Satoshis(999), EmptyScriptPubKey)
    val ignoredOutput =
      TransactionOutput(Bitcoins.one,
                        NonStandardScriptPubKey(Vector(OP_RETURN)))
    val dustTx = BaseTransaction(tx.version,
                                 tx.inputs,
                                 Vector(ignoredOutput, newOutput),
                                 tx.lockTime)

    assert(
      Transaction
        .sanityAmountChecks(forSigned = true, inputInfos, feeRate, dustTx)
        .isFailure)
  }
}
