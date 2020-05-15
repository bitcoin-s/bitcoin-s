package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  NonStandardScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{EmptyInputInfo, ScriptSignatureParams}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class TxBuilderTest extends BitcoinSUnitTest {

  "TxBuilder" must "detect a bad fee on the tx" in {
    val estimatedFee = 1000.sats
    val actualFee = 1.sat
    val feeRate = SatoshisPerVirtualByte(1.sat)
    TxBuilder
      .isValidFeeRange(estimatedFee, actualFee, feeRate)
      .isFailure must be(true)

    TxBuilder
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

  private val txBuilderF = BitcoinTxBuilder(
    Vector(output),
    Vector(
      ScriptSignatureParams(EmptyInputInfo(outPoint,
                                           Bitcoins.one + Bitcoins.one),
                            Vector.empty,
                            HashType.sigHashAll)),
    SatoshisPerVirtualByte(Satoshis.one),
    EmptyScriptPubKey,
    RegTest
  )

  private val txBuilder = Await.result(txBuilderF, 5.seconds)

  it must "detect a missing destination" in {
    val missingOutputTx = BaseTransaction(tx.version,
                                          tx.inputs,
                                          Vector.empty[TransactionOutput],
                                          tx.lockTime)

    assert(TxBuilder.sanityChecks(txBuilder, missingOutputTx).isFailure)
  }

  it must "detect extra outputs added" in {
    val newOutput = TransactionOutput(Bitcoins.max, EmptyScriptPubKey)
    val extraOutputTx = BaseTransaction(tx.version,
                                        tx.inputs,
                                        Vector(output, newOutput),
                                        tx.lockTime)

    assert(TxBuilder.sanityChecks(txBuilder, extraOutputTx).isFailure)
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

    assert(TxBuilder.sanityChecks(txBuilder, extraOutPointTx).isFailure)
  }

  it must "detect impossibly high fees" in {
    val newOutput = TransactionOutput(Bitcoins.zero, EmptyScriptPubKey)
    val highFeeTx =
      BaseTransaction(tx.version, tx.inputs, Vector(newOutput), tx.lockTime)

    assert(TxBuilder.sanityAmountChecks(txBuilder, highFeeTx).isFailure)
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

    assert(TxBuilder.sanityAmountChecks(txBuilder, dustTx).isFailure)
  }
}
