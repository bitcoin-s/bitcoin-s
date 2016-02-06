package org.scalacoin.script.splice

import org.scalacoin.script.{ScriptOperationFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant.{ScriptNumberImpl, OP_0, ScriptConstantImpl}
import org.scalacoin.util.ScalacoinUtil
import Math._
/**
 * Created by chris on 2/4/16.
 */
trait SpliceInterpreter {

  /**
   * Pushes the string length of the top element of the stack (without popping it).
   * @param program
   * @return
   */
  def opSize(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SIZE, "Script top must be OP_SIZE")
    require(program.stack.size > 0, "Must have at least 1 element on the stack for OP_SIZE")
/*    val stringSize = program.stack.head.bytes.size
    val scriptNumber = if (stringSize == 0) OP_0 else ScriptNumberImpl(stringSize)
    ScriptProgramImpl(scriptNumber :: program.stack, program.script.tail, program.transaction,program.altStack)*/
    if (program.stack.head == OP_0) {
      ScriptProgramImpl(OP_0 :: program.stack, program.script.tail, program.transaction, program.altStack)
    } else {
      val stringSize = ScalacoinUtil.hexToLong(program.stack.head.hex)
      val intSize = bytes(stringSize)
      val scriptNumber = ScriptNumberImpl(intSize)
      ScriptProgramImpl(scriptNumber :: program.stack, program.script.tail, program.transaction,program.altStack)
    }

  }

  def bytes(x: Long): Int = {
    val posx = x.abs
    if (posx == 0L) 0
    else (64 - java.lang.Long.numberOfLeadingZeros(posx)) / 8 + 1
  }
}
