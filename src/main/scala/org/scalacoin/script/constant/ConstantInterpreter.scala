package org.scalacoin.script.constant

import org.scalacoin.util.ScalacoinUtil
import org.slf4j.LoggerFactory

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
    val numberOfBytesToBePushed : ScriptToken = script(1)
    val numberOfBytes : Int = Integer.parseInt(numberOfBytesToBePushed.hex,16)
    val slicedScript = script.slice(2,script.size)
    println("Slice script: " + slicedScript)
    println("Number of bytes: " + numberOfBytes)
    val (newStack,newScript) = opPushData(stack,slicedScript,numberOfBytes)
    (newStack,newScript)
  }

  /**
   * The next two bytes contain the number of bytes to be pushed onto the stack.
   * @param stack
   * @param script
   * @return
   */
  def opPushData2(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_PUSHDATA2, "Top of script stack must be OP_PUSHDATA2")
    val numberOfBytesToBePushed : String = script.slice(1,3).map(_.hex).mkString
    logger.debug("Number of bytes to be pushed: " + numberOfBytesToBePushed)
    val numberOfBytes : Int = Integer.parseInt(numberOfBytesToBePushed,16)
    logger.debug("Parsed hex: " + numberOfBytes)
    val slicedScript = script.slice(3,script.size)
    val (newStack,newScript) = opPushData(stack,slicedScript,numberOfBytes)
    (newStack,newScript)
  }

  /**
   * The next four bytes contain the number of bytes to be pushed onto the stack.
   * @param stack
   * @param script
   * @return
   */
  def opPushData4(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_PUSHDATA4, "Top of script stack must be OP_PUSHDATA4")
    val numberOfBytesToBePushed : String = script.slice(1,5).map(_.hex).mkString
    val numberOfBytes : Int = Integer.parseInt(numberOfBytesToBePushed,16)
    val slicedScript = script.slice(5,script.size)
    val (newStack,newScript) = opPushData(stack,slicedScript,numberOfBytes)
    (newStack,newScript)
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
