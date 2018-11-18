package org.bitcoins.core.script.stack

import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BitcoinSLogger

import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 1/6/16.
 * Stack operations implemented in the script programming language
 * https://en.bitcoin.it/wiki/Script#Stack
 */
sealed abstract class StackInterpreter {
  private def logger = BitcoinSLogger.logger

  /**
   * Duplicates the element on top of the stack
   * expects the first element in script to be the OP_DUP operation.
   */
  def opDup(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_DUP), "Top of the script stack must be OP_DUP")
    program.stack match {
      case h :: t => ScriptProgram(program, h :: program.stack, program.script.tail)
      case Nil =>
        logger.error("Cannot duplicate the top element on an empty stack")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** If the top stack value is not 0, duplicate it. */
  def opIfDup(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_IFDUP), "Top of the script stack must be OP_DUP")
    if (program.stack.nonEmpty) {
      if (program.stack.head == ScriptNumber.zero) return ScriptProgram(program, program.stack, program.script.tail)
      ScriptProgram(program, program.stack.head :: program.stack, program.script.tail)
    } else {
      logger.error("Cannot duplicate the top element on an empty stack")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Puts the number of stack items onto the stack. */
  def opDepth(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_DEPTH), "Top of script stack must be OP_DEPTH")
    val stackSize = program.stack.size
    val numberToPush: ScriptNumber = ScriptNumber(stackSize)
    ScriptProgram(program, numberToPush :: program.stack, program.script.tail)
  }

  /** Puts the input onto the top of the alt stack. Removes it from the main stack. */
  def opToAltStack(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_TOALTSTACK), "Top of script stack must be OP_TOALTSTACK")
    if (program.stack.nonEmpty) {
      ScriptProgram(program, program.stack.tail,
        program.script.tail, program.stack.head :: program.altStack, ScriptProgram.AltStack)
    } else {
      logger.error("OP_TOALTSTACK requires an element to be on the stack")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Puts the input onto the top of the main stack. Removes it from the alt stack. */
  def opFromAltStack(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_FROMALTSTACK), "Top of script stack must be OP_FROMALTSTACK")
    if (program.altStack.nonEmpty) {
      ScriptProgram(program, program.altStack.head :: program.stack,
        program.script.tail, program.altStack.tail, ScriptProgram.AltStack)
    } else {
      logger.error("Alt Stack must have at least one item on it for OP_FROMALTSTACK")
      ScriptProgram(program, ScriptErrorInvalidAltStackOperation)
    }
  }

  /** Removes the top stack item. */
  def opDrop(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_DROP), "Top of script stack must be OP_DROP")
    if (program.stack.nonEmpty) {
      ScriptProgram(program, program.stack.tail, program.script.tail)
    } else {
      logger.error("Stack must have at least one item on it for OP_DROP")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Removes the second-to-top stack item. */
  def opNip(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_NIP), "Top of script stack must be OP_NIP")
    program.stack match {
      case h :: _ :: t => ScriptProgram(program, h :: t, program.script.tail)
      case h :: t =>
        logger.error("Stack must have at least two items on it for OP_NIP")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
      case Nil =>
        logger.error("Stack must have at least two items on it for OP_NIP")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Copies the second-to-top stack item to the top. */
  def opOver(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_OVER), "Top of script stack must be OP_OVER")
    program.stack match {
      case _ :: h1 :: _ => ScriptProgram(program, h1 :: program.stack, program.script.tail)
      case h :: t =>
        logger.error("Stack must have at least two items on it for OP_OVER")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
      case Nil =>
        logger.error("Stack must have at least two items on it for OP_OVER")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** The item n back in the stack is copied to the top. */
  def opPick(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_PICK), "Top of script stack must be OP_PICK")
    executeOpWithStackTopAsNumberArg(program, { number: ScriptNumber =>

      //check if n is within the bound of the script
      if (program.stack.size < 2) ScriptProgram(program, ScriptErrorInvalidStackOperation)
      else if (number.toLong >= 0 && number.toLong < program.stack.tail.size) {
        val newStackTop = program.stack.tail(number.toInt)
        ScriptProgram(program, newStackTop :: program.stack.tail, program.script.tail)
      } else {
        logger.error("The index for OP_PICK would have caused an index out of bounds exception")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
      }
    })
  }

  /** The item n back in the stack is moved to the top. */
  def opRoll(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_ROLL), "Top of script stack must be OP_ROLL")
    executeOpWithStackTopAsNumberArg(program, (number: ScriptNumber) =>
      if (program.stack.size < 2) ScriptProgram(program, ScriptErrorInvalidStackOperation)
      else if (number.toLong >= 0 && number.toLong < program.stack.tail.size) {
        val newStackTop = program.stack.tail(number.toInt)
        //removes the old instance of the stack top, appends the new index to the head
        val newStack = newStackTop :: program.stack.tail.diff(List(newStackTop))
        ScriptProgram(program, newStack, program.script.tail)
      } else {
        logger.error("The index for OP_ROLL would have caused an index out of bounds exception")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
      })
  }

  /**
   * The top three items on the stack are rotated to the left.
   * Ex: x1 x2 x3 -> x2 x3 x1
   */
  def opRot(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_ROT), "Top of script stack must be OP_ROT")
    program.stack match {
      case h :: h1 :: h2 :: t =>
        val newStack = h2 :: h :: h1 :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 3 items on it for OP_ROT")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /**
   * The fifth and sixth items back are moved to the top of the stack.
   * Ex. x1 x2 x3 x4 x5 x6 -> x3 x4 x5 x6 x1 x2
   */
  def op2Rot(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_2ROT), "Top of script stack must be OP_2ROT")
    program.stack match {
      case h :: h1 :: h2 :: h3 :: h4 :: h5 :: t =>
        val newStack = h4 :: h5 :: h :: h1 :: h2 :: h3 :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("OP_2ROT requires 6 elements on the stack")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Removes the top two stack items. */
  def op2Drop(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_2DROP), "Top of script stack must be OP_2DROP")
    if (program.stack.size > 1) {
      ScriptProgram(program, program.stack.tail.tail, program.script.tail)
    } else {
      logger.error("OP_2DROP requires two elements to be on the stack")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** The top two items on the stack are swapped. */
  def opSwap(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_SWAP), "Top of script stack must be OP_SWAP")
    if (program.stack.size > 1) {
      val newStack = program.stack.tail.head :: program.stack.head :: program.stack.tail.tail
      ScriptProgram(program, newStack, program.script.tail)
    } else {
      logger.error("Stack must have at least 2 items on it for OP_SWAP")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** The item at the top of the stack is copied and inserted before the second-to-top item. */
  def opTuck(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_TUCK), "Top of script stack must be OP_TUCK")
    program.stack match {
      case h :: h1 :: t =>
        val newStack = h :: h1 :: h :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 2 items on it for OP_TUCK")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Duplicates the top two stack items. */
  def op2Dup(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_2DUP), "Top of script stack must be OP_2DUP")
    program.stack match {
      case h :: h1 :: t =>
        val newStack = h :: h1 :: h :: h1 :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 2 items on it for OP_2DUP")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Duplicates the top three stack items. */
  def op3Dup(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_3DUP), "Top of script stack must be OP_3DUP")
    program.stack match {
      case h :: h1 :: h2 :: t =>
        val newStack = h :: h1 :: h2 :: h :: h1 :: h2 :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 3 items on it for OP_3DUP")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Copies the pair of items two spaces back in the stack to the front. */
  def op2Over(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_2OVER), "Top of script stack must be OP_2OVER")
    program.stack match {
      case h :: h1 :: h2 :: h3 :: t =>
        val newStack = h2 :: h3 :: h :: h1 :: h2 :: h3 :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 4 items on it for OP_2OVER")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /** Swaps the top two pairs of items. */
  def op2Swap(program: ScriptProgram): ScriptProgram = {
    require(program.script.headOption.contains(OP_2SWAP), "Top of script stack must be OP_2SWAP")
    program.stack match {
      case h :: h1 :: h2 :: h3 :: t =>
        val newStack = h2 :: h3 :: h :: h1 :: t
        ScriptProgram(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 4 items on it for OP_2SWAP")
        ScriptProgram(program, ScriptErrorInvalidStackOperation)
    }
  }

  /**
   * Executes an operation with the stack top inside of the program as the argument
   * @param program the program whose stack top is used as an argument for the operation
   * @param op the operation that is executed with the script number on the top of the stack
   * @return the program with the result of the op pushed onto to the top of the stack
   */
  private def executeOpWithStackTopAsNumberArg(program: ScriptProgram, op: ScriptNumber => ScriptProgram): ScriptProgram = {
    program.stack.head match {
      case scriptNum: ScriptNumber => op(scriptNum)
      case _: ScriptToken =>
        //interpret the stack top as a number
        val number: Try[ScriptNumber] = ScriptNumber(program.stack.head.bytes, ScriptFlagUtil.requireMinimalData(program.flags))
        number match {
          case Success(n) => op(n)
          case Failure(_) =>
            logger.error("Script number was not minimally encoded")
            ScriptProgram(program, ScriptErrorUnknownError)
        }
    }
  }
}

object StackInterpreter extends StackInterpreter
