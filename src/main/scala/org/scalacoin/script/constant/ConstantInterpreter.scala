package org.scalacoin.script.constant

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.util.BitcoinSUtil
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/24/16.
 */
trait ConstantInterpreter {

  private def logger = LoggerFactory.getLogger(this.getClass())


  /**
   * The next byte contains the number of bytes to be pushed onto the stack.
   * @param program
   * @return
   */
  def opPushData1(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA1, "Top of script stack must be OP_PUSHDATA1")

    val bytesToPush = program.script(1)
    if (bytesToPush == BytesToPushOntoStackImpl(0)) {
      ScriptProgramFactory.factory(program, OP_0 :: program.stack, program.script.tail.tail)
    } else {
      //leave the constant on the script to be pushed onto the stack by the next iteration of script interpreter
      //TODO: probably push the script token on the stack here, but it needs to be converted to the
      //appropriate type i.e. OP_1 is converted to ScriptNumberImpl(1)
      ScriptProgramFactory.factory(program, program.stack, program.script.slice(2,program.script.size))
    }

  }

  /**
   * The next two bytes contain the number of bytes to be pushed onto the stack.
   * @param program
   * @return
   */
  def opPushData2(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA2, "Top of script stack must be OP_PUSHDATA2")
    //convert the hex string from little endian to big endian
    val bytesToPush = program.script(1)
    if (bytesToPush == BytesToPushOntoStackImpl(0)) {
      ScriptProgramFactory.factory(program, OP_0 :: program.stack, program.script.tail.tail)
    } else {
      //leave the constant on the script to be pushed onto the stack by the next iteration of script interpreter
      //TODO: probably push the script token on the stack here, but it needs to be converted to the
      //appropriate type i.e. OP_1 is converted to ScriptNumberImpl(1)
      ScriptProgramFactory.factory(program, program.stack, program.script.slice(2,program.script.size))
    }
  }

  /**
   * The next four bytes contain the number of bytes to be pushed onto the stack.
   * @param program
   * @return
   */
  def opPushData4(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PUSHDATA4, "Top of script stack must be OP_PUSHDATA4")
    //convert the hex string from little endian to big endian

    val bytesToPush = program.script(1)
    if (bytesToPush == BytesToPushOntoStackImpl(0)) {
      ScriptProgramFactory.factory(program, OP_0 :: program.stack, program.script.tail.tail)
    } else {
      //leave the constant on the script to be pushed onto the stack by the next iteration of script interpreter
      //TODO: probably push the script token on the stack here, but it needs to be converted to the
      //appropriate type i.e. OP_1 is converted to ScriptNumberImpl(1)
      ScriptProgramFactory.factory(program, program.stack, program.script.slice(2,program.script.size))
    }
  }


  /**
   * Pushes the number of bytes onto the stack that is specified by script number on the script stack
   * @param program
   * @return
   */
  def pushScriptNumberBytesToStack(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head.isInstanceOf[BytesToPushOntoStack], "Top of script must be a script number")
    require(program.script.size > 1, "Script size must be atleast to to push constants onto the stack")
    val bytesNeeded = program.script.head match {
      case scriptNumber : BytesToPushOntoStack => scriptNumber.opCode
      case _ => throw new IllegalArgumentException("Stack top must be BytesToPushOntoStack to push a numbero bytes onto the stack")
    }
    /**
     * Parses the script tokens that need to be pushed onto our stack
     * @param scriptTokens
     * @param accum
     * @return
     */
    @tailrec
    def takeUntilBytesNeeded(scriptTokens : List[ScriptToken], accum : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
      val bytesSum = accum.map(_.bytesSize).sum
      if (bytesSum == bytesNeeded) (scriptTokens,accum)
      else if (scriptTokens.size == 0) (Nil,accum)
      else if (bytesSum > bytesNeeded) throw new RuntimeException("We cannot have more bytes than what our script number specified")
      else {
        //for the case when a ScriptNumberImpl(x) was parsed as a ByteToPushOntoStackImpl(x)
        val scriptToken = scriptTokens.head match {
          case BytesToPushOntoStackImpl(x) => ScriptNumberImpl(x)
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

    logger.debug("Constant: " + constant)
    ScriptProgramFactory.factory(program, constant :: program.stack, newScript)
  }


}
