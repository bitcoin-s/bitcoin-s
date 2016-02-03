package org.scalacoin.script.constant

import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.util.ScalacoinUtil
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
    val numberOfBytes : Int = Integer.parseInt(program.script(1).hex,16)
    if (numberOfBytes == 0) {
      //if the number of bytes pushed onto the stack is zero, we push an empty byte vector onto the stack
      ScriptProgramImpl(OP_0 :: program.stack, program.script.slice(2,program.script.size),program.transaction)
    } else {
      val slicedScript = program.script.slice(2,program.script.size)
      val (newStack,newScript) = opPushData(program.stack,slicedScript,numberOfBytes)
      ScriptProgramImpl(newStack,newScript,program.transaction)
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
    val reversedHex = ScalacoinUtil.littleEndianToBigEndian(program.script(1).hex)
    val numberOfBytes : Int = Integer.parseInt(reversedHex,16)
    if (numberOfBytes == 0) {
      //if the number of bytes pushed onto the stack is zero, we push an empty byte vector onto the stack
      ScriptProgramImpl(OP_0 :: program.stack, program.script.slice(2,program.script.size),program.transaction)
    } else {
      val slicedScript = program.script.slice(2,program.script.size)
      val (newStack,newScript) = opPushData(program.stack,slicedScript,numberOfBytes)
      ScriptProgramImpl(newStack,newScript,program.transaction)
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
    val reversedHex = ScalacoinUtil.littleEndianToBigEndian(program.script(1).hex)
    val numberOfBytes : Int = Integer.parseInt(reversedHex,16)
    if (numberOfBytes == 0) {
      //if the number of bytes pushed onto the stack is zero, we push an empty byte vector onto the stack
      ScriptProgramImpl(OP_0 :: program.stack, program.script.slice(2,program.script.size), program.transaction)
    } else {
      val slicedScript = program.script.slice(2,program.script.size)
      val (newStack,newScript) = opPushData(program.stack,slicedScript,numberOfBytes)
      ScriptProgramImpl(newStack,newScript,program.transaction)
    }
  }


  /**
   * Pushes the number of bytes onto the stack that is specified by script number on the script stack
   * @param program
   * @return
   */
  def pushScriptNumberBytesToStack(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head.isInstanceOf[ScriptNumber], "Top of script must be a script number")
    require(program.script.size > 1, "Script size must be atleast to to push constants onto the stack")
    val bytesNeeded = program.script.head match {
      case scriptNumber : ScriptNumber => scriptNumber.opCode
    }
    /**
     * Parses the script tokens that need to be pushed onto our stack
     * @param scriptToken
     * @param accum
     * @return
     */
    @tailrec
    def takeUntilBytesNeeded(scriptTokens : List[ScriptToken], scriptConstantAccum : ScriptConstant) : (List[ScriptToken],ScriptConstant) = {
      val bytesSum = scriptConstantAccum.bytesSize
      if (bytesSum == bytesNeeded) (scriptTokens,scriptConstantAccum)
      else if (bytesSum > bytesNeeded) throw new RuntimeException("We cannot have more bytes than what our script number specified")
      else takeUntilBytesNeeded(scriptTokens.tail, ScriptConstantImpl(scriptConstantAccum.hex + scriptTokens.head.hex))
    }

    val (newScript,newScriptConstant) = takeUntilBytesNeeded(program.script.tail,ScriptConstantImpl(""))
    //see if the new script constant can be converted into a script number
    val scriptNumber : Option[ScriptNumber] = ScriptNumberFactory.fromHex(newScriptConstant.hex)
    if (scriptNumber.isDefined) ScriptProgramImpl(scriptNumber.get :: program.stack, newScript,program.transaction)
    else ScriptProgramImpl(newScriptConstant :: program.stack, newScript, program.transaction)
  }

  /**
   * Responsible for pushing the amount of bytes specified by the param numberOfBytes onto the stack
   * @param stack
   * @param script
   * @param numberOfBytes
   * @return
   */
  private def opPushData(stack : List[ScriptToken], script : List[ScriptToken],numberOfBytes : Int) : (List[ScriptToken],List[ScriptToken]) = {
    val tokensToBePushed : List[ScriptToken] = script.slice(0,numberOfBytes)
    val newStack = tokensToBePushed.reverse ++ stack
    val newScript = script.slice(numberOfBytes,script.size)
    (newStack,newScript)
  }

}
