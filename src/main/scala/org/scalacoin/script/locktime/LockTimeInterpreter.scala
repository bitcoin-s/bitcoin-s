package org.scalacoin.script.locktime

import org.scalacoin.script.constant.{ScriptNumberImpl, ScriptNumber}
import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}

/**
 * Created by chris on 2/8/16.
 */
trait LockTimeInterpreter {


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
      ScriptProgramImpl(program.stack, program.script.tail, program.transaction, program.altStack,false)
    } else {
      val isValid = program.stack.head match {
        case s : ScriptNumber if (s < ScriptNumberImpl(0)) => false
        case s : ScriptNumber if (s > ScriptNumberImpl(500000000) && program.transaction.lockTime < 500000000) => false
        case s : ScriptNumber if (s < ScriptNumberImpl(500000000) && program.transaction.lockTime > 500000000) => false
        case s if (program.transaction.inputs.map(_.sequence == 0xffffffff).exists(_ == true)) => false
        case _ => true
      }
      ScriptProgramImpl(program.stack, program.script.tail, program.transaction, program.altStack,isValid)
    }
  }

}
