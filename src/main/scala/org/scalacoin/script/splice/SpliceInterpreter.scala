package org.scalacoin.script.splice

import org.scalacoin.script.{ScriptOperationFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalacoin.util.ScalacoinUtil

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
    val stringSize = program.stack.head.bytes.size
    val hex = ScalacoinUtil.encodeHex(stringSize.toByte)
    val scriptOperation = ScriptOperationFactory.fromHex(hex)
    val size = if (scriptOperation.isDefined) scriptOperation.get else ScriptConstantImpl(hex)
    ScriptProgramImpl(size :: program.stack, program.script.tail, program.transaction,program.altStack)
  }

}
