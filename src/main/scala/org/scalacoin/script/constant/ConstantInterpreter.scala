package org.scalacoin.script.constant

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgram}
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/24/16.
 */
trait ConstantInterpreter extends BitcoinSLogger {

  /**
   * The next byte contains the number of bytes to be pushed onto the stack.
   * @param program
   * @return
   */
  def opPushData1(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA1, "Top of script stack must be OP_PUSHDATA1")
    pushScriptNumberBytesToStack(ScriptProgramFactory.factory(program,program.script.tail, ScriptProgramFactory.Script))
  }

  /**
   * The next two bytes contain the number of bytes to be pushed onto the stack.
   * @param program
   * @return
   */
  def opPushData2(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA2, "Top of script stack must be OP_PUSHDATA2")
    pushScriptNumberBytesToStack(ScriptProgramFactory.factory(program,program.script.tail, ScriptProgramFactory.Script))
  }

  /**
   * The next four bytes contain the number of bytes to be pushed onto the stack.
   * @param program
   * @return
   */
  def opPushData4(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA4, "Top of script stack must be OP_PUSHDATA4")
    pushScriptNumberBytesToStack(ScriptProgramFactory.factory(program,program.script.tail, ScriptProgramFactory.Script))
  }


  /**
   * Pushes the number of bytes onto the stack that is specified by script number on the script stack
   * @param program
   * @return
   */
  def pushScriptNumberBytesToStack(program : ScriptProgram) : ScriptProgram = {
    val bytesNeeded : Long = program.script.head match {
      case scriptNumber : BytesToPushOntoStack => scriptNumber.opCode
      case scriptNumber : ScriptNumber => scriptNumber.num
      case _ => throw new IllegalArgumentException("Stack top must be BytesToPushOntoStack to push a number of bytes onto the stack")
    }
    /**
     * Parses the script tokens that need to be pushed onto our stack
     * @param scriptTokens
     * @param accum
     * @return
     */
    @tailrec
    def takeUntilBytesNeeded(scriptTokens : List[ScriptToken], accum : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
      val bytesSum = accum.map(_.bytes.size).sum
      if (bytesSum == bytesNeeded) (scriptTokens,accum)
      else if (scriptTokens.size == 0) (Nil,accum)
      else if (bytesSum > bytesNeeded) throw new RuntimeException("We cannot have more bytes than what our script number specified")
      else {
        //for the case when a ScriptNumberImpl(x) was parsed as a ByteToPushOntoStackImpl(x)
        val scriptToken = scriptTokens.head match {
          case x : BytesToPushOntoStack => ScriptNumberFactory.fromNumber(x.opCode)
          case x => x
        }
        takeUntilBytesNeeded(scriptTokens.tail, scriptToken :: accum)
      }
    }

    val (newScript,bytesToPushOntoStack) = takeUntilBytesNeeded(program.script.tail,List())
    logger.debug("new script: " + newScript)
    logger.debug("Bytes to push onto stack" + bytesToPushOntoStack)
    val constant : ScriptToken = if (bytesToPushOntoStack.size == 1) bytesToPushOntoStack.head
    else ScriptConstantFactory.fromHex(BitcoinSUtil.flipEndianess(bytesToPushOntoStack.flatMap(_.bytes)))

    logger.debug("Constant to be pushed onto stack: " + constant)
    //check to see if we have the exact amount of bytes needed to be pushed onto the stack
    //if we do not, mark the program as invalid
    if (bytesNeeded == 0) ScriptProgramFactory.factory(program, ScriptNumberFactory.zero :: program.stack, newScript)
    else if (bytesNeeded != bytesToPushOntoStack.map(_.bytes.size).sum) ScriptProgramFactory.factory(program,false)
    else ScriptProgramFactory.factory(program, constant :: program.stack, newScript)
  }


}
