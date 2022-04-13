package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkitcore.util.TestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 3/30/16.
  */
class TransactionOutPointTest extends BitcoinSUnitTest {
  "TransactionOutPoint" must "define an empty transaction outpoint" in {
    EmptyTransactionOutPoint.txId.hex must be(
      "0000000000000000000000000000000000000000000000000000000000000000")
    EmptyTransactionOutPoint.vout must be(UInt32.max)
  }

  it must "read then write a transaction outpoint" in {

    val outPoint = TestUtil.simpleTransaction.inputs.head.previousOutput
    TransactionOutPoint(outPoint.hex).hex must be(outPoint.hex)
  }

  it must "read a transaction outpoint from string" in {
    val txIdBE = DoubleSha256DigestBE(
      "1d8a6f050746882216518afac933f5c0139e288fbdc3fea8de627b886b0d68cf")
    val string = s"${txIdBE.hex}:1"
    val expected = TransactionOutPoint(txIdBE, UInt32.one)

    assert(TransactionOutPoint.fromString(string) == expected)
  }
}
