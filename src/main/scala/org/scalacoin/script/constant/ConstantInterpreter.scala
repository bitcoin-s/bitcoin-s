package org.scalacoin.script.constant

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
   * @param stack
   * @param script
   */
  def opPushData1(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_PUSHDATA1, "Top of script stack must be OP_PUSHDATA1")
    val numberOfBytes : Int = Integer.parseInt(script(1).hex,16)
    if (numberOfBytes == 0) {
      //if the number of bytes pushed onto the stack is zero, we push an empty byte vector onto the stack
      (OP_0 :: stack, script.slice(2,script.size))
    } else {
      val slicedScript = script.slice(2,script.size)
      val (newStack,newScript) = opPushData(stack,slicedScript,numberOfBytes)
      (newStack,newScript)
    }
  }

  /**
   * The next two bytes contain the number of bytes to be pushed onto the stack.
   * @param stack
   * @param script
   * @return
   */
  def opPushData2(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_PUSHDATA2, "Top of script stack must be OP_PUSHDATA2")
    //convert the hex string from little endian to big endian
    val reversedHex = ScalacoinUtil.littleEndianToBigEndian(script(1).hex)
    val numberOfBytes : Int = Integer.parseInt(reversedHex,16)
    if (numberOfBytes == 0) {
      //if the number of bytes pushed onto the stack is zero, we push an empty byte vector onto the stack
      (OP_0 :: stack, script.slice(2,script.size))
    } else {
      val slicedScript = script.slice(2,script.size)
      val (newStack,newScript) = opPushData(stack,slicedScript,numberOfBytes)
      (newStack,newScript)
    }
  }

  /**
   * The next four bytes contain the number of bytes to be pushed onto the stack.
   * @param stack
   * @param script
   * @return
   */
  def opPushData4(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_PUSHDATA4, "Top of script stack must be OP_PUSHDATA4")
    //convert the hex string from little endian to big endian
    val reversedHex = ScalacoinUtil.littleEndianToBigEndian(script(1).hex)
    val numberOfBytes : Int = Integer.parseInt(reversedHex,16)
    if (numberOfBytes == 0) {
      //if the number of bytes pushed onto the stack is zero, we push an empty byte vector onto the stack
      (OP_0 :: stack, script.slice(2,script.size))
    } else {
      val slicedScript = script.slice(2,script.size)
      val (newStack,newScript) = opPushData(stack,slicedScript,numberOfBytes)
      (newStack,newScript)
    }
  }


  /**
   * Pushes the number of bytes onto the stack that is specified by script number on the script stack
   * @param stack
   * @param script
   * @return
   */
  def pushScriptNumberBytesToStack(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head.isInstanceOf[ScriptNumber], "Top of script must be a script number")
    require(script.size > 1, "Script size must be atleast to to push constants onto the stack")
    val bytesNeeded = script.head match {
      case scriptNumber : ScriptNumber => scriptNumber.opCode
    }
    /**
     * Parses the script tokens that need to be pushed onto our stack
     * @param scriptToken
     * @param accum
     * @return
     */
    @tailrec
    def takeUntilBytesNeeded(scriptTokens : List[ScriptToken], accum : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
      if (accum.map(_.bytes.size).sum == bytesNeeded) (scriptTokens,accum)
      else if (accum.map(_.bytes.size).sum > bytesNeeded) throw new RuntimeException("We cannot have more bytes than what our script number specified")
      else takeUntilBytesNeeded(scriptTokens.tail, scriptTokens.head :: accum)
    }

    val (newScript,tokensToBePushed) = takeUntilBytesNeeded(script.tail,List())
    (tokensToBePushed ++ stack, newScript)

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
