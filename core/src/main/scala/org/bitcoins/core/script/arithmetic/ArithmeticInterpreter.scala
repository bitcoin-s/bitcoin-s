package org.bitcoins.core.script.arithmetic

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{
  ControlOperationsInterpreter,
  OP_VERIFY
}
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  ExecutionInProgressScriptProgram,
  StartedScriptProgram
}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

import scala.annotation.tailrec

/**
  * Created by chris on 1/25/16.
  */
sealed abstract class ArithmeticInterpreter {
  private def logger = BitcoinSLogger.logger

  /** a is added to b. */
  def opAdd(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_ADD),
            "Script top must be OP_ADD")
    performBinaryArithmeticOperation(program, (x, y) => x + y)
  }

  /** Increments the stack top by 1. */
  def op1Add(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_1ADD),
            "Script top must be OP_1ADD")
    performUnaryArithmeticOperation(program, x => x + ScriptNumber.one)
  }

  /** Decrements the stack top by 1. */
  def op1Sub(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_1SUB),
            "Script top must be OP_1SUB")
    performUnaryArithmeticOperation(program, x => x - ScriptNumber.one)
  }

  /** b is subtracted from a. */
  def opSub(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_SUB),
            "Script top must be OP_SUB")
    performBinaryArithmeticOperation(program, (x, y) => y - x)
  }

  /** Takes the absolute value of the stack top. */
  def opAbs(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_ABS),
            "Script top must be OP_ABS")
    performUnaryArithmeticOperation(
      program,
      x =>
        x match {
          case ScriptNumber.zero => ScriptNumber.zero
          case _: ScriptNumber   => ScriptNumber(x.toLong.abs)
        })
  }

  /** Negates the stack top. */
  def opNegate(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_NEGATE),
            "Script top must be OP_NEGATE")
    performUnaryArithmeticOperation(program, x => -x)
  }

  /** If the input is 0 or 1, it is flipped. Otherwise the output will be 0. */
  def opNot(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_NOT),
            "Script top must be OP_NOT")
    performUnaryArithmeticOperation(
      program,
      _ => if (program.stackTopIsFalse) OP_TRUE else OP_FALSE)
  }

  /** Returns 0 if the input is 0. 1 otherwise. */
  def op0NotEqual(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_0NOTEQUAL),
            "Script top must be OP_0NOTEQUAL")
    performUnaryArithmeticOperation(
      program,
      x => if (x.toLong == 0) OP_FALSE else OP_TRUE)
  }

  /** If both a and b are not 0, the output is 1. Otherwise 0. */
  def opBoolAnd(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_BOOLAND),
            "Script top must be OP_BOOLAND")
    performBinaryBooleanOperation(
      program,
      (x, y) => {
        !ScriptNumberUtil.isZero(x) && !ScriptNumberUtil.isZero(y)
      }
    )
  }

  /** If a or b is not 0, the output is 1. Otherwise 0. */
  def opBoolOr(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_BOOLOR),
            "Script top must be OP_BOOLOR")
    performBinaryBooleanOperation(
      program,
      (x, y) => {
        !ScriptNumberUtil.isZero(x) || !ScriptNumberUtil.isZero(y)
      })
  }

  /** Returns 1 if the numbers are equal, 0 otherwise. */
  def opNumEqual(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_NUMEQUAL),
            "Script top must be OP_NUMEQUAL")
    performBinaryBooleanOperation(program, (x, y) => x.numEqual(y))
  }

  /** Same as [[org.bitcoins.core.script.arithmetic.OP_NUMEQUAL OP_NUMEQUAL]], but runs
    * [[org.bitcoins.core.script.control.OP_VERIFY OP_VERIFY]] afterward.
    */
  def opNumEqualVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_NUMEQUALVERIFY),
            "Script top must be OP_NUMEQUALVERIFY")
    if (program.stack.size < 2) {
      logger.error("OP_NUMEQUALVERIFY requires two stack elements")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val numEqualProgram = program.updateStackAndScript(
        program.stack,
        OP_NUMEQUAL :: program.script.tail)
      val numEqualResultOrError = opNumEqual(numEqualProgram)
      numEqualResultOrError match {
        case numEqualResult: ExecutionInProgressScriptProgram =>
          val verifyProgram = numEqualResult.updateStackAndScript(
            numEqualResult.stack,
            OP_VERIFY :: numEqualResult.script)
          val verifyResult =
            ControlOperationsInterpreter.opVerify(verifyProgram)
          verifyResult
        case err: ExecutedScriptProgram =>
          err
      }
    }
  }

  /** Returns 1 if the numbers are not equal, 0 otherwise. */
  def opNumNotEqual(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_NUMNOTEQUAL),
            "Script top must be OP_NUMNOTEQUAL")
    performBinaryBooleanOperation(program,
                                  (x, y) => {
                                    x.toLong != y.toLong
                                  })
  }

  /** Returns 1 if a is less than b, 0 otherwise. */
  def opLessThan(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_LESSTHAN),
            "Script top must be OP_LESSTHAN")
    performBinaryBooleanOperation(program, (x, y) => y < x)
  }

  /** Returns 1 if a is greater than b, 0 otherwise. */
  def opGreaterThan(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_GREATERTHAN),
            "Script top must be OP_GREATERTHAN")
    performBinaryBooleanOperation(program, (x, y) => y > x)
  }

  /** Returns 1 if a is less than or equal to b, 0 otherwise. */
  def opLessThanOrEqual(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_LESSTHANOREQUAL),
            "Script top must be OP_LESSTHANOREQUAL")
    performBinaryBooleanOperation(program, (x, y) => y <= x)
  }

  /** Returns 1 if a is greater than or equal to b, 0 otherwise. */
  def opGreaterThanOrEqual(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_GREATERTHANOREQUAL),
            "Script top must be OP_GREATERTHANOREQUAL")
    performBinaryBooleanOperation(program, (x, y) => y >= x)
  }

  /** Returns the smaller of a and b. */
  def opMin(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_MIN),
            "Script top must be OP_MIN")
    if (program.stack.size < 2) {
      logger.error("OP_MAX requires at least two stack elements")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      performComparisonOnTwoStackTopItems(
        program,
        (x: ScriptNumber, y: ScriptNumber) => if (x < y) x else y)
    }
  }

  /** Returns the larger of a and b. */
  def opMax(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_MAX),
            "Script top must be OP_MAX")
    if (program.stack.size < 2) {
      logger.error("OP_MAX requires at least two stack elements")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      performComparisonOnTwoStackTopItems(
        program,
        (x: ScriptNumber, y: ScriptNumber) => if (x > y) x else y)
    }
  }

  /** Returns 1 if x is within the specified range (left-inclusive), 0 otherwise. */
  def opWithin(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_WITHIN),
            "Script top must be OP_WITHIN")
    if (program.stack.size < 3) {
      logger.error("OP_WITHIN requires at least 3 elements on the stack")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val c = ScriptNumber(program.stack.head.bytes)
      val b = ScriptNumber(program.stack.tail.head.bytes)
      val a = ScriptNumber(program.stack.tail.tail.head.bytes)
      if (
        ScriptFlagUtil.requireMinimalData(program.flags) && (!BitcoinScriptUtil
          .isShortestEncoding(c) ||
        !BitcoinScriptUtil.isShortestEncoding(b) || !BitcoinScriptUtil
          .isShortestEncoding(a))
      ) {
        logger.error(
          "The constant you gave us is not encoded in the shortest way possible")
        program.failExecution(ScriptErrorUnknownError)
      } else if (
        isLargerThan4Bytes(c) || isLargerThan4Bytes(b) || isLargerThan4Bytes(a)
      ) {
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error(
          "Cannot perform arithmetic operation on a number larger than 4 bytes, one of these three numbers is larger than 4 bytes: "
            + c + " " + b + " " + a)
        program.failExecution(ScriptErrorUnknownError)
      } else {
        val isWithinRange = a >= b && a < c
        val newStackTop = if (isWithinRange) OP_TRUE else OP_FALSE
        program.updateStackAndScript(
          newStackTop :: program.stack.tail.tail.tail,
          program.script.tail)
      }
    }
  }

  /**
    * This function checks if a number is <= 4 bytes in size
    * We cannot perform arithmetic operations on bitcoin numbers that are larger than 4 bytes.
    * https://github.com/bitcoin/bitcoin/blob/a6a860796a44a2805a58391a009ba22752f64e32/src/script/script.h#L214-L239.
    */
  private def isLargerThan4Bytes(scriptNumber: ScriptNumber): Boolean =
    scriptNumber.bytes.size > 4

  /**
    * Performs the given arithmetic operation on the stack head
    * @param program the program whose stack top is used as an argument for the arithmetic operation
    * @param op the arithmetic ooperation that needs to be executed on the number, for instance incrementing by 1
    * @return the program with the result from performing the arithmetic operation pushed onto the top of the stack
    */
  @tailrec
  private def performUnaryArithmeticOperation(
      program: ExecutionInProgressScriptProgram,
      op: ScriptNumber => ScriptNumber): StartedScriptProgram = {
    program.stack.headOption match {
      case None =>
        logger.error(
          "We need one stack element for performing a unary arithmetic operation")
        program.failExecution(ScriptErrorInvalidStackOperation)
      case Some(s: ScriptNumber) =>
        if (
          ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil
            .isShortestEncoding(s)
        ) {
          logger.error(
            "The number you gave us is not encoded in the shortest way possible")
          program.failExecution(ScriptErrorMinimalData)
        } else if (isLargerThan4Bytes(s)) {
          logger.error(
            "Cannot perform arithmetic operation on a number larger than 4 bytes, here is the number: " + s)
          //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
          //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
          program.failExecution(ScriptErrorUnknownError)
        } else {
          val newScriptNumber = op(s)
          program.updateStackAndScript(newScriptNumber :: program.stack.tail,
                                       program.script.tail)
        }
      case Some(s: ScriptConstant) =>
        if (
          ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil
            .isShortestEncoding(s)
        ) {
          logger.error(
            "The number you gave us is not encoded in the shortest way possible")
          program.failExecution(ScriptErrorUnknownError)
        } else {
          val interpretedNumber = ScriptNumber(ScriptNumberUtil.toLong(s.hex))
          val newProgram =
            program.updateStack(interpretedNumber :: program.stack.tail)
          performUnaryArithmeticOperation(newProgram, op)
        }
      case Some(_: ScriptToken) =>
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error(
          "Stack top must be a script number/script constant to perform an arithmetic operation")
        program.failExecution(ScriptErrorUnknownError)
    }
  }

  /**
    * Performs the given arithmetic operation on the top two stack items
    * @param program the program whose stack top is used as an argument for the arithmetic operation
    * @param op the arithmetic ooperation that needs to be executed on the number, for instance incrementing by 1
    * @return the program with the result from performing the arithmetic operation pushed onto the top of the stack
    */
  @tailrec
  private def performBinaryArithmeticOperation(
      program: ExecutionInProgressScriptProgram,
      op: (
          ScriptNumber,
          ScriptNumber) => ScriptNumber): StartedScriptProgram = {
    if (program.stack.size < 2) {
      logger.error(
        "We must have two elements to perform a binary arithmetic operation")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      (program.stack.head, program.stack.tail.head) match {
        case (x: ScriptNumber, y: ScriptNumber) =>
          if (
            ScriptFlagUtil.requireMinimalData(
              program.flags) && (!BitcoinScriptUtil
              .isShortestEncoding(x) || !BitcoinScriptUtil.isShortestEncoding(
              y))
          ) {
            logger.error(
              "The constant you gave us is not encoded in the shortest way possible")
            program.failExecution(ScriptErrorUnknownError)
          } else if (isLargerThan4Bytes(x) || isLargerThan4Bytes(y)) {
            //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
            //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
            logger.error(
              "Cannot perform arithmetic operation on a number larger than 4 bytes, one of these two numbers is larger than 4 bytes: " + x + " " + y)
            program.failExecution(ScriptErrorUnknownError)
          } else {
            val newStackTop = op(x, y)
            program.updateStackAndScript(newStackTop :: program.stack.tail.tail,
                                         program.script.tail)
          }
        case (x: ScriptConstant, _: ScriptNumber) =>
          //interpret x as a number
          val interpretedNumber = ScriptNumber(x.hex)
          val newProgram =
            program.updateStack(interpretedNumber :: program.stack.tail)
          performBinaryArithmeticOperation(newProgram, op)
        case (x: ScriptNumber, y: ScriptConstant) =>
          val interpretedNumber = ScriptNumber(y.hex)
          val newProgram =
            program.updateStack(x :: interpretedNumber :: program.stack.tail)
          performBinaryArithmeticOperation(newProgram, op)
        case (x: ScriptConstant, y: ScriptConstant) =>
          //interpret x and y as a number
          val interpretedNumberX = ScriptNumber(x.hex)
          val interpretedNumberY = ScriptNumber(y.hex)
          val newProgram = program.updateStack(
            interpretedNumberX :: interpretedNumberY :: program.stack.tail.tail)
          performBinaryArithmeticOperation(newProgram, op)
        case (_: ScriptToken, _: ScriptToken) =>
          //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
          //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
          logger.error(
            "The top two stack items must be script numbers to perform an arithmetic operation")
          program.failExecution(ScriptErrorUnknownError)
      }
    }
  }

  /**
    * Compares two script numbers with the given boolean operation
    * @param program the program whose two top stack elements are used for the comparison
    * @param op the operation which compares the two script numbers
    * @return the program with either OP_FALSE or OP_TRUE on the stack top
    */
  private def performBinaryBooleanOperation(
      program: ExecutionInProgressScriptProgram,
      op: (ScriptNumber, ScriptNumber) => Boolean): StartedScriptProgram = {
    if (program.stack.size < 2) {
      logger.error("We need two stack elements for a binary boolean operation")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val (x, y) = parseTopTwoStackElementsAsScriptNumbers(program)
      if (
        ScriptFlagUtil.requireMinimalData(program.flags) &&
        (!BitcoinScriptUtil.isShortestEncoding(x) || !BitcoinScriptUtil
          .isShortestEncoding(y))
      ) {
        logger.error(
          "The constant you gave us is not encoded in the shortest way possible")
        program.failExecution(ScriptErrorUnknownError)
      } else if (isLargerThan4Bytes(x) || isLargerThan4Bytes(y)) {
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error(
          "Cannot perform boolean operation on a number larger than 4 bytes, one of these two numbers is larger than 4 bytes: " + x + " " + y)
        program.failExecution(ScriptErrorUnknownError)
      } else {
        val newStackTop = if (op(x, y)) OP_TRUE else OP_FALSE
        program.updateStackAndScript(newStackTop :: program.stack.tail.tail,
                                     program.script.tail)
      }
    }
  }

  /**
    * Takes the top two stack items, parses them to numbers then executes the op function on them and places the result
    * onto the stack top
    * @param program the script program whose two top stack items are used as arguments for op
    * @param op the operation that needs to be executed on the two stack top items
    * @return the program with the result of op pushed onto the top of the stack
    */
  private def performComparisonOnTwoStackTopItems(
      program: ExecutionInProgressScriptProgram,
      op: (
          ScriptNumber,
          ScriptNumber) => ScriptNumber): StartedScriptProgram = {
    performBinaryArithmeticOperation(program, op)
  }

  /**
    * Takes the top two stack elements and parses them as script numbers
    * @param program the program whose top two stack elements are being parsed as script numbers
    * @return the tuple with the first element being the first stack element, the second element in the tuple being the second stack element
    */
  private def parseTopTwoStackElementsAsScriptNumbers(
      program: ExecutionInProgressScriptProgram): (
      ScriptNumber,
      ScriptNumber) = {
    (program.stack.head, program.stack.tail.head) match {
      case (x: ScriptNumber, y: ScriptNumber) => (x, y)
      case (x: ScriptConstant, y: ScriptNumber) =>
        val interpretedNumber = ScriptNumber(x.hex)
        (interpretedNumber, y)
      case (x: ScriptNumber, y: ScriptConstant) =>
        //interpret y as a number
        val interpretedNumber = ScriptNumber(y.hex)
        (x, interpretedNumber)
      case (x: ScriptConstant, y: ScriptConstant) =>
        //interpret x and y as a number
        val interpretedNumberX = ScriptNumber(x.hex)
        val interpretedNumberY = ScriptNumber(y.hex)
        (interpretedNumberX, interpretedNumberY)
      case (_: ScriptToken, _: ScriptToken) =>
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error(
          "The top two stack items must be script numbers to perform an arithmetic operation")
        throw new RuntimeException(
          "Stack top elements must have be script constants to be interpreted as numbers")
    }
  }
}

object ArithmeticInterpreter extends ArithmeticInterpreter
