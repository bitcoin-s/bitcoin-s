package org.bitcoins.script.locktime

import org.bitcoins.protocol.transaction.TransactionConstants
import org.bitcoins.script.constant.{ScriptToken, ScriptNumberImpl, ScriptNumber}
import org.bitcoins.script.error.{ScriptError, ScriptErrorNegativeLockTime, ScriptErrorUnsatisfiedLocktime, ScriptErrorInvalidStackOperation}
import org.bitcoins.script.{ScriptProgram}
import org.bitcoins.util.BitcoinSLogger

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
 *
   * @param program
   * @return
   */
  def opCheckLockTimeVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKLOCKTIMEVERIFY,
      "Script top must be OP_CHECKLOCKTIMEVERIFY")
    if (program.stack.size == 0) {
      logger.warn("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because we have no stack items")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    } else if (program.txSignatureComponent.transaction.inputs(program.txSignatureComponent.inputIndex).sequence == TransactionConstants.sequence) {
      logger.warn("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because the sequence number is 0xffffffff")
      ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
    }
    else {
      val isError : Option[ScriptError] = program.stack.head match {
        case s : ScriptNumber if (s < ScriptNumber.zero) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks tx as invalid if the stack top is negative")
          Some(ScriptErrorNegativeLockTime)
        case s : ScriptNumber if (s >= ScriptNumber(500000000) && program.txSignatureComponent.transaction.lockTime < 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top >= 500000000 & tx locktime < 500000000")
          Some(ScriptErrorUnsatisfiedLocktime)
        case s : ScriptNumber if (s < ScriptNumber(500000000) && program.txSignatureComponent.transaction.lockTime >= 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top < 500000000 & tx locktime >= 500000000")
          Some(ScriptErrorUnsatisfiedLocktime)
        case _ : ScriptToken => None
      }
      if (isError.isDefined) ScriptProgram(program,isError.get)
      else ScriptProgram(program,program.stack, program.script.tail)
    }
  }

}
