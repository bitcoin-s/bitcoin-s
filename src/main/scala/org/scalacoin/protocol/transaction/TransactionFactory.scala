package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionParser, RawTransactionInputParser}
import org.scalacoin.util.Factory

/**
 * Created by chris on 2/21/16.
 */

sealed trait TransactionFactoryHelper
case class UpdateTransactionOutputs(outputs : Seq[TransactionOutput]) extends TransactionFactoryHelper
case class UpdateTransactionInputs(inputs : Seq[TransactionInput]) extends TransactionFactoryHelper
