package org.scalacoin.script.locktime

import org.scalacoin.protocol.transaction.TransactionConstants
import org.scalacoin.script.constant.{ScriptNumberImpl, ScriptNumber}
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.util.BitcoinSLogger

/**
 * Created by chris on 2/8/16.
 */
trait LockTimeInterpreter extends BitcoinSLogger {


  /**
   * Marks transaction as invalid if the top stack item is greater than the transaction's nLockTime field,
   * otherwise script evaluation continues as though an OP_NOP was executed. Transaction is also invalid if
   * 1. the stack is empty; or
   * 2. the top stack item is negative; or
   * 3. the top stack item is greater than or equal to 500000000 while the transaction's nLockTime field is less than 500000000,
   * or vice versa; or
   * 4. the input's nSequence field is equal to 0xffffffff.
   * The precise semantics are described in BIP 0065
   * @param program
   * @return
   */
  def opCheckLockTimeVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKLOCKTIMEVERIFY,
      "Script top must be OP_CHECKLOCKTIMEVERIFY")
    if (program.stack.size == 0) {
      logger.warn("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because we have no stack items")
      ScriptProgramFactory.factory(program, program.stack, program.script.tail, false)
    } else if (program.transaction.inputs(program.inputIndex).sequence == TransactionConstants.sequence) {
      logger.warn("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because the sequence number is 0xffffffff")
      ScriptProgramFactory.factory(program, program.stack, program.script.tail, false)
    }
    else {
      val isValid = program.stack.head match {
        case s : ScriptNumber if (s < ScriptNumberImpl(0)) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks tx as invalid if the stack top is negative")
          false
        case s : ScriptNumber if (s >= ScriptNumberImpl(500000000) && program.transaction.lockTime < 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top >= 500000000 & tx locktime < 500000000")
          false
        case s : ScriptNumber if (s < ScriptNumberImpl(500000000) && program.transaction.lockTime >= 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top < 500000000 & tx locktime >= 500000000")
          false
        case _ => true
      }
      ScriptProgramFactory.factory(program,program.stack, program.script.tail, isValid)
    }
  }

}
