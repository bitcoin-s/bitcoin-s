package org.bitcoins.protocol.transaction

import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionInputFactoryTest extends FlatSpec with MustMatchers {

  "TransactionInputFactory" must "create a transaction input out of it's base components" in {

    val input = TransactionInput(EmptyTransactionInput.previousOutput,EmptyTransactionInput.scriptSignature, EmptyTransactionInput.sequence)
    input.previousOutput must be (EmptyTransactionInput.previousOutput)
    input.scriptSignature must be (EmptyTransactionInput.scriptSignature)
    input.sequence must be (EmptyTransactionInput.sequence)
  }

  it must "modify an input from an output and that output's transaction" in {
    val input = TestUtil.txInput.head
    val newInput = TransactionInput(input,TestUtil.simpleTransaction.outputs(0), TestUtil.simpleTransaction)

    newInput.previousOutput.txId must be (TestUtil.simpleTransaction.txId)
    newInput.previousOutput.vout must be (0)
  }

  it must "chage the input's outpoint to the given outpoint" in {
    val input = TestUtil.txInput.head
    val newInput = TransactionInput(input,EmptyTransactionOutPoint)
    newInput.previousOutput must be (EmptyTransactionOutPoint)

  }
}
