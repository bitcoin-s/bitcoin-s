package org.bitcoins.core.script.constant

import org.bitcoins.core.script.{
  ExecutionInProgressScriptProgram,
  StartedScriptProgram
}
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil, BytesUtil}

import scala.annotation.tailrec

/**
  * Created by chris on 1/24/16.
  */
sealed abstract class ConstantInterpreter extends BitcoinSLogger {

  /** The next byte contains the number of bytes to be pushed onto the stack. */
  def opPushData1(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_PUSHDATA1),
            "Top of script stack must be OP_PUSHDATA1")
    opPushData(program)
  }

  /** The next two bytes contain the number of bytes to be pushed onto the stack. */
  def opPushData2(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_PUSHDATA2),
            "Top of script stack must be OP_PUSHDATA2")
    opPushData(program)
  }

  /** The next four bytes contain the number of bytes to be pushed onto the stack. */
  def opPushData4(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_PUSHDATA4),
            "Top of script stack must be OP_PUSHDATA4")
    opPushData(program)
  }

  /** Pushes the number of bytes onto the stack that is specified by script number on the script stack. */
  def pushScriptNumberBytesToStack(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    val bytesNeeded: Long = program.script.head match {
      case OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4 =>
        bytesNeededForPushOp(program.script(1))
      case _: ScriptToken => bytesNeededForPushOp(program.script.head)
    }

    /** Parses the script tokens that need to be pushed onto our stack. */
    @tailrec
    def takeUntilBytesNeeded(
        scriptTokens: List[ScriptToken],
        accum: List[ScriptToken]): (List[ScriptToken], List[ScriptToken]) = {
      val bytesSum = accum.map(_.bytes.size).sum
      if (bytesSum == bytesNeeded) (scriptTokens, accum)
      else if (scriptTokens.isEmpty) (Nil, accum)
      else if (bytesSum > bytesNeeded)
        throw new RuntimeException(
          "We cannot have more bytes than what our script number specified")
      else {
        //for the case when a ScriptNumberImpl(x) was parsed as a ByteToPushOntoStackImpl(x)
        val scriptToken = scriptTokens.head match {
          case x: BytesToPushOntoStack => ScriptNumber(x.opCode)
          case x                       => x
        }
        takeUntilBytesNeeded(scriptTokens.tail, scriptToken :: accum)
      }
    }

    val (newScript, bytesToPushOntoStack) = program.script.head match {
      case OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4 =>
        takeUntilBytesNeeded(program.script.tail.tail, Nil)
      case _: ScriptToken => takeUntilBytesNeeded(program.script.tail, Nil)
    }
    logger.debug("new script: " + newScript)
    logger.debug("Bytes to push onto stack: " + bytesToPushOntoStack)
    val constant: ScriptToken =
      if (bytesToPushOntoStack.size == 1) bytesToPushOntoStack.head
      else
        ScriptConstant(
          BytesUtil.flipEndianness(
            BytesUtil.toByteVector(bytesToPushOntoStack)))

    logger.debug("Constant to be pushed onto stack: " + constant)
    //check to see if we have the exact amount of bytes needed to be pushed onto the stack
    //if we do not, mark the program as invalid
    if (bytesNeeded == 0)
      program.updateStackAndScript(ScriptNumber.zero :: program.stack,
                                   newScript)
    else if (
      ScriptFlagUtil.requireMinimalData(program.flags) && bytesNeeded == 1 &&
      constant.isInstanceOf[ScriptNumber] && constant.toLong <= 16
    ) {
      logger.error(
        "We can push this constant onto the stack with OP_0 - OP_16 instead of using a script constant")
      program.failExecution(ScriptErrorMinimalData)
    } else if (bytesNeeded != bytesToPushOntoStack.map(_.bytes.size).sum) {
      logger.error("Incorrect amount of bytes being pushed onto the stack")
      logger.error("Bytes needed: " + bytesNeeded)
      logger.error(
        "Number of byte received: " + bytesToPushOntoStack
          .map(_.bytes.size)
          .sum)
      program.failExecution(ScriptErrorBadOpCode)
    } else if (
      ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil
        .isMinimalPush(program.script.head, constant)
    ) {
      logger.debug("Pushing operation: " + program.script.head)
      logger.debug("Constant parsed: " + constant)
      logger.debug("Constant size: " + constant.bytes.size)
      program.failExecution(ScriptErrorMinimalData)
    } else program.updateStackAndScript(constant :: program.stack, newScript)
  }

  /**
    * Checks if the MINIMALDATA script flag is set, if so checks if we are using the minimal push operation
    * if we are, then we push the bytes onto the stack.
    */
  private def opPushData(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    //for the case where we have to push 0 bytes onto the stack, which is technically the empty byte vector
    def emptyPush(): StartedScriptProgram = {
      if (ScriptFlagUtil.requireMinimalData(program.flags)) {
        program.failExecution(ScriptErrorMinimalData)
      } else {
        program.updateStackAndScript(ScriptNumber.zero :: program.stack,
                                     program.script.tail.tail)
      }
    }

    program.script(1) match {
      case OP_0 | ScriptNumber.zero | ScriptNumber.negativeZero =>
        emptyPush()
      case token: ScriptConstant if token.bytes.toSeq.forall(_ == 0.toByte) =>
        emptyPush()
      case _: ScriptToken =>
        if (
          ScriptFlagUtil.requireMinimalData(
            program.flags) && program.script.size > 2 && !BitcoinScriptUtil
            .isMinimalPush(program.script.head, program.script(2))
        ) {
          logger.error(
            s"Non-minimal push where minimal push was required: $program")
          program.failExecution(ScriptErrorMinimalData)
        } else {
          pushScriptNumberBytesToStack(program)
        }
    }
  }

  /** Parses the bytes needed for a push op (for instance OP_PUSHDATA1). */
  private def bytesNeededForPushOp(token: ScriptToken): Long =
    token match {
      case scriptNumber: BytesToPushOntoStack => scriptNumber.opCode
      case scriptNumOp: ScriptNumberOperation => scriptNumOp.opCode
      case scriptNumber: ScriptNumber         => scriptNumber.toLong
      case scriptConstant: ScriptConstant =>
        val constantFlippedEndianness =
          BytesUtil.flipEndianness(scriptConstant.hex)
        java.lang.Long.parseLong(constantFlippedEndianness, 16)
      case _ =>
        throw new IllegalArgumentException(
          "Token must be BytesToPushOntoStack to push a number of bytes onto the stack")
    }
}

object ConstantInterpreter extends ConstantInterpreter
