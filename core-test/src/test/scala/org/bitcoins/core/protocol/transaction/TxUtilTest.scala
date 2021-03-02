package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  NonStandardScriptPubKey
}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.EmptyInputInfo
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class TxUtilTest extends BitcoinSUnitTest {
  behavior of "TxUtil"

  private val outPoint =
    TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero)

  private val input =
    TransactionInput(outPoint, EmptyScriptSignature, UInt32.zero)
  private val output = TransactionOutput(Bitcoins.one, EmptyScriptPubKey)

  private val tx = BaseTransaction(TransactionConstants.validLockVersion,
                                   Vector(input),
                                   Vector(output),
                                   UInt32.zero)

  private val inputInfos = Vector(
    EmptyInputInfo(outPoint, Bitcoins.one + Bitcoins.one))
  private val feeRate = SatoshisPerVirtualByte(Satoshis.one)

  it should "detect a bad fee on the tx" in {
    val estimatedFee = 1000.sats
    val actualFee = 1.sat
    val feeRate = SatoshisPerVirtualByte(1.sat)
    TxUtil
      .isValidFeeRange(estimatedFee, actualFee, feeRate)
      .isFailure must be(true)

    TxUtil
      .isValidFeeRange(actualFee, estimatedFee, feeRate)
      .isFailure must be(true)
  }

  it should "detect impossibly high fees" in {
    val newOutput = TransactionOutput(Bitcoins.zero, EmptyScriptPubKey)
    val highFeeTx =
      BaseTransaction(tx.version, tx.inputs, Vector(newOutput), tx.lockTime)

    assert(
      TxUtil
        .sanityAmountChecks(isSigned = true, inputInfos, feeRate, highFeeTx)
        .isFailure)
  }

  it should "detect dust outputs" in {
    val newOutput = TransactionOutput(Satoshis(999), EmptyScriptPubKey)
    val ignoredOutput =
      TransactionOutput(Bitcoins.one,
                        NonStandardScriptPubKey(Vector(OP_RETURN)))
    val dustTx = BaseTransaction(tx.version,
                                 tx.inputs,
                                 Vector(ignoredOutput, newOutput),
                                 tx.lockTime)

    assert(
      TxUtil
        .sanityAmountChecks(isSigned = true, inputInfos, feeRate, dustTx)
        .isFailure)
  }
}
