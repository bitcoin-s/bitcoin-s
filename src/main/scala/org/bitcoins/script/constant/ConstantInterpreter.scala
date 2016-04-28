package org.bitcoins.script.constant

import org.bitcoins.script.error.{ScriptErrorMinimalData, ScriptErrorInvalidStackOperation}
import org.bitcoins.script.flag.{ScriptFlagUtil, ScriptVerifyMinimalData}
import org.bitcoins.script.ScriptProgram
import org.bitcoins.util.{BitcoinScriptUtil, BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/24/16.
 */
trait ConstantInterpreter extends BitcoinSLogger {

  /**
   * The next byte contains the number of bytes to be pushed onto the stack.
 *
   * @param program
   * @return
   */
  def opPushData1(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA1, "Top of script stack must be OP_PUSHDATA1")
    opPushData(program)
  }

  /**
   * The next two bytes contain the number of bytes to be pushed onto the stack.
 *
   * @param program
   * @return
   */
  def opPushData2(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA2, "Top of script stack must be OP_PUSHDATA2")
    opPushData(program)
  }

  /**
   * The next four bytes contain the number of bytes to be pushed onto the stack.
 *
   * @param program
   * @return
   */
  def opPushData4(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA4, "Top of script stack must be OP_PUSHDATA4")
    opPushData(program)
  }


  /**
   * Pushes the number of bytes onto the stack that is specified by script number on the script stack
 *
   * @param program
   * @return
   */
  def pushScriptNumberBytesToStack(program : ScriptProgram) : ScriptProgram = {


    val bytesNeeded : Long = program.script.head match {
      case OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4 =>
        bytesNeededForPushOp(program.script(1))
      case _ : ScriptToken => bytesNeededForPushOp(program.script.head)
    }

    /**
     * Parses the script tokens that need to be pushed onto our stack
 *
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
          case x : BytesToPushOntoStack => ScriptNumber(x.opCode)
          case x => x
        }
        takeUntilBytesNeeded(scriptTokens.tail, scriptToken :: accum)
      }
    }

    val (newScript,bytesToPushOntoStack) = program.script.head match {
      case OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4 => takeUntilBytesNeeded(program.script.tail.tail, List())
      case _: ScriptToken => takeUntilBytesNeeded(program.script.tail, List())
    }
    logger.debug("new script: " + newScript)
    logger.debug("Bytes to push onto stack" + bytesToPushOntoStack)
    val constant : ScriptToken = if (bytesToPushOntoStack.size == 1) bytesToPushOntoStack.head
    else ScriptConstant(BitcoinSUtil.flipEndianess(bytesToPushOntoStack.flatMap(_.bytes)))

    logger.debug("Constant to be pushed onto stack: " + constant)
    //check to see if we have the exact amount of bytes needed to be pushed onto the stack
    //if we do not, mark the program as invalid
    if (bytesNeeded == 0) ScriptProgram(program, ScriptNumber.zero :: program.stack, newScript)
/*    else if (ScriptFlagUtil.requireMinimalData(program.flags) && bytesNeeded == 1 &&
      constant.isInstanceOf[ScriptNumber] &&constant.toLong <= 16) {
      logger.error("We can push this constant onto the stack with OP_0 - OP_16 instead of using a script constant")
      ScriptProgram(program,ScriptErrorMinimalData)
    }*/
    else if (bytesNeeded != bytesToPushOntoStack.map(_.bytes.size).sum) ScriptProgram(program,ScriptErrorInvalidStackOperation)
    else {
      if (ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil.isMinimalPush(program.script.head,constant)) {
        logger.debug("Pushing operation: " + program.script.head)
        logger.debug("Constant parsed: " + constant)
        logger.debug("Constant size: " + constant.bytes.size)
        ScriptProgram(program,ScriptErrorMinimalData)
      } else ScriptProgram(program, constant :: program.stack, newScript)
    }
  }


  /**
   * Checks if the MINIMALDATA script flag is set, if so checks if we are using the minimal push operation
   * if we are, then we push the bytes onto the stack
 *
   * @param program
   * @return
   */
  private def opPushData(program : ScriptProgram) : ScriptProgram = {

    //for the case when we have the minimal data flag and the bytes to push onto stack is represented by the
    //constant telling OP_PUSHDATA how many bytes need to go onto the stack
    //for instance OP_PUSHDATA1 OP_0
    val scriptNumOp = program.script(1).bytes match {
      case h :: t => ScriptNumberOperation(h)
      case Nil => None
    }
    if (ScriptFlagUtil.requireMinimalData(program.flags) && program.script(1).bytes.size == 1 &&
      scriptNumOp.isDefined) {
      logger.error("We cannot use an OP_PUSHDATA operation for pushing " +
        "a script number operation onto the stack, scriptNumberOperation: " + scriptNumOp)
      ScriptProgram(program,ScriptErrorMinimalData)
    } else if (ScriptFlagUtil.requireMinimalData(program.flags) && program.script.size > 2 &&
      !BitcoinScriptUtil.isMinimalPush(program.script.head, program.script(2))) {
      logger.error("We are not using the minimal push operation to push the bytes onto the stack for the constant")
      ScriptProgram(program, ScriptErrorMinimalData)
    } else {
      //for the case where we have to push 0 bytes onto the stack, which is technically the empty byte vector
      program.script(1) match {
        case OP_0 | BytesToPushOntoStack.zero | ScriptNumber.zero
             | ScriptNumber.negativeZero if (ScriptFlagUtil.requireMinimalData(program.flags)) =>
          ScriptProgram(program,ScriptErrorMinimalData)
        case OP_0 | BytesToPushOntoStack.zero | ScriptNumber.zero | ScriptNumber.negativeZero =>
          ScriptProgram(program, ScriptNumber.zero :: program.stack, program.script.tail.tail)
        case _ : ScriptToken =>
          pushScriptNumberBytesToStack(ScriptProgram(program, program.script, ScriptProgram.Script))
      }
    }
  }

  /**
   * Parses the bytes needed for a push op (for instance OP_PUSHDATA1)
 *
   * @param token
   * @return
   */
  private def bytesNeededForPushOp(token : ScriptToken) : Long = token match {
    case scriptNumber: BytesToPushOntoStack => scriptNumber.opCode
    case scriptNumber: ScriptNumber => scriptNumber.num
    case _ => throw new IllegalArgumentException("Token must be BytesToPushOntoStack to push a number of bytes onto the stack")
  }
}
