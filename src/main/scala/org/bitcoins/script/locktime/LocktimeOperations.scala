package org.bitcoins.script.locktime

import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.constant.ScriptOperation

/**
 * Created by chris on 1/6/16.
 */
sealed trait LocktimeOperation extends ScriptOperation

/**
 * Marks transaction as invalid if the top stack item is greater than the transaction's nLockTime field,
 * otherwise script evaluation continues as though an OP_NOP was executed. Transaction is also invalid if
 * 1. the stack is empty; or
 * 2. the top stack item is negative; or
 * 3. the top stack item is greater than or equal to 500000000 while the transaction's nLockTime field is less than 500000000,
 * or vice versa; or 4. the input's nSequence field is equal to 0xffffffff.
 * The precise semantics are described in BIP 0065
 */
case object OP_CHECKLOCKTIMEVERIFY extends LocktimeOperation {
  override def opCode = 177
}

object LocktimeOperation extends ScriptOperationFactory[LocktimeOperation] {
  override def operations = Seq(OP_CHECKLOCKTIMEVERIFY)
}