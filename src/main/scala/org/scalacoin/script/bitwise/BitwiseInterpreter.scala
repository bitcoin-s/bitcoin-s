package org.scalacoin.script.bitwise

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.script.control.{OP_VERIFY, ControlOperationsInterpreter}
import org.scalacoin.util.BitcoinSUtil
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/6/16.
 */
trait BitwiseInterpreter extends ControlOperationsInterpreter  {

  /**
   * Returns 1 if the inputs are exactly equal, 0 otherwise.
   * @param program
   * @return
   */
  def opEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.size > 1, "Stack size must be 2 or more to compare the top two values for OP_EQUAL")
    require(program.script.headOption.isDefined && program.script.head == OP_EQUAL, "Script operation must be OP_EQUAL")

    logger.debug("Original stack: " + program.stack)
    val h = program.stack.head
    val h1 = program.stack.tail.head

    val result = (h,h1) match {
      case (OP_0, x) => OP_0.hex == x.hex
      case (x, OP_0) => x.hex == OP_0.hex
      case (OP_1,x) => OP_1.scriptNumber == x
      case (x,OP_1) => x == OP_1.scriptNumber
/*      case (x : ScriptConstant, y : ScriptConstant) => x == y
      case (ScriptConstantImpl(x), y : ScriptNumber) => BitcoinSUtil.hexToLong(x) == y.num
      case (x : ScriptNumber, y : ScriptConstant) => x.num == BitcoinSUtil.hexToLong(y.hex)*/
      case _ => h.bytes == h1.bytes
    }

    val scriptBoolean : ScriptBoolean = if (result) ScriptTrue else ScriptFalse

    ScriptProgramFactory.factory(program,scriptBoolean :: program.stack.tail.tail, program.script.tail)
  }


  /**
   * Same as OP_EQUAL, but runs OP_VERIFY afterward.
   * @param program
   * @return
   */
  def opEqualVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(program.script.headOption.isDefined && program.script.head == OP_EQUALVERIFY, "Script operation must be OP_EQUALVERIFY")
    //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
    val simpleScript = OP_EQUAL :: OP_VERIFY :: program.script.tail
    val newProgram: ScriptProgram = opEqual(ScriptProgramFactory.factory(program, program.stack, simpleScript))
    opVerify(newProgram)
  }


}
