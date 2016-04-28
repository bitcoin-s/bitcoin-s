package org.bitcoins.script.arithmetic

import org.bitcoins.script.control.{ControlOperationsInterpreter, OP_VERIFY}
import org.bitcoins.script.error.{ScriptErrorMinimalData, ScriptErrorUnknownError, ScriptErrorInvalidStackOperation}
import org.bitcoins.script.flag.ScriptFlagUtil
import org.bitcoins.script.{ExecutedScriptProgram, PreExecutionScriptProgram, ExecutionInProgressScriptProgram, ScriptProgram}
import org.bitcoins.script.constant._
import org.bitcoins.util.{BitcoinScriptUtil, BitcoinSUtil}

import scala.annotation.tailrec

/**
 * Created by chris on 1/25/16.
 */
trait ArithmeticInterpreter extends ControlOperationsInterpreter {


  /**
   * a is added to b
 *
   * @param program
   * @return
   */
  def opAdd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ADD, "Script top must be OP_ADD")
    performBinaryArithmeticOperation(program, (x,y) => x + y)
  }

  /**
   * Increments the stack top by 1
 *
   * @param program
   * @return
   */
  def op1Add(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1ADD, "Script top must be OP_1ADD")
    performUnaryArithmeticOperation(program, x => x + ScriptNumber.one)
  }

  /**
   * Decrements the stack top by 1
 *
   * @param program
   * @return
   */
  def op1Sub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1SUB, "Script top must be OP_1SUB")
    performUnaryArithmeticOperation(program, x => x - ScriptNumber.one )
  }


  /**
   * b is subtracted from a.
 *
   * @param program
   * @return
   */
  def opSub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SUB, "Script top must be OP_SUB")
    performBinaryArithmeticOperation(program, (x,y) => y - x)
  }

  /**
   * Takes the absolute value of the stack top
 *
   * @param program
   * @return
   */
  def opAbs(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ABS, "Script top must be OP_ABS")
    performUnaryArithmeticOperation(program, x => x match {
      case ScriptNumber.zero => ScriptNumber.zero
      case _ : ScriptNumber => ScriptNumber(x.num.abs)
    })
  }

  /**
   * Negates the stack top
 *
   * @param program
   * @return
   */
  def opNegate(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NEGATE, "Script top must be OP_NEGATE")
    performUnaryArithmeticOperation(program, x => x -)
  }

  /**
   * If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
 *
   * @param program
   * @return
   */
  def opNot(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NOT, "Script top must be OP_NOT")

    performUnaryArithmeticOperation(program, x => if (program.stackTopIsFalse) OP_TRUE else OP_FALSE)
  }

  /**
   * Returns 0 if the input is 0. 1 otherwise.
 *
   * @param program
   * @return
   */
  def op0NotEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_0NOTEQUAL, "Script top must be OP_0NOTEQUAL")
    performUnaryArithmeticOperation(program, x => if(x.num == 0) OP_FALSE else OP_TRUE)
  }


  /**
   * 	If both a and b are not 0, the output is 1. Otherwise 0.
 *
   * @param program
   * @return
   */
  def opBoolAnd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLAND, "Script top must be OP_BOOLAND")
    performBinaryBooleanOperation(program,(x,y) => {
      val xIsFalse = (x == ScriptNumber.zero || x == OP_0)
      val yIsFalse = (y == ScriptNumber.zero || y == OP_0)
      if (xIsFalse || yIsFalse) false else true
    })

  }

  /**
   * If a or b is not 0, the output is 1. Otherwise 0.
 *
   * @param program
   * @return
   */
  def opBoolOr(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLOR, "Script top must be OP_BOOLOR")
    performBinaryBooleanOperation(program, (x,y) => {
      if (x == y && (x == ScriptNumber.zero || x == OP_0)) false else true
    })
  }

  /**
   * Returns 1 if the numbers are equal, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opNumEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMEQUAL, "Script top must be OP_NUMEQUAL")
    performBinaryBooleanOperation(program,(x,y) => x.numEqual(y))
  }


  /**
   * Same as OP_NUMEQUAL, but runs OP_VERIFY afterward.
 *
   * @param program
   * @return
   */
  def opNumEqualVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMEQUALVERIFY,
      "Script top must be OP_NUMEQUALVERIFY")
    if (program.stack.size < 2) {
      logger.error("OP_NUMEQUALVERIFY requires two stack elements")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val numEqualProgram = ScriptProgram(program, program.stack, OP_NUMEQUAL :: program.script.tail)
      val numEqualResult = opNumEqual(numEqualProgram)
      numEqualResult match {
        case _ : ExecutionInProgressScriptProgram =>
          val verifyProgram = ScriptProgram(numEqualResult, numEqualResult.stack, OP_VERIFY :: numEqualResult.script)
          val verifyResult = opVerify(verifyProgram)
          verifyResult
        case _ : PreExecutionScriptProgram | _ : ExecutedScriptProgram =>
          numEqualResult
      }
    }
  }


  /**
   * 	Returns 1 if the numbers are not equal, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opNumNotEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMNOTEQUAL,
      "Script top must be OP_NUMNOTEQUAL")
    performBinaryBooleanOperation(program, (x,y) => {
      x.num != y.num
    })
  }


  /**
   * 	Returns 1 if a is less than b, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opLessThan(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_LESSTHAN,
      "Script top must be OP_LESSTHAN")
    performBinaryBooleanOperation(program, (x,y) => y < x)
  }


  /**
   * 	Returns 1 if a is greater than b, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opGreaterThan(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_GREATERTHAN,
      "Script top must be OP_GREATERTHAN")
    performBinaryBooleanOperation(program, (x,y) => y > x)
  }

  /**
   * Returns 1 if a is less than or equal to b, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opLessThanOrEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_LESSTHANOREQUAL,
      "Script top must be OP_LESSTHANOREQUAL")
    performBinaryBooleanOperation(program, (x,y) => y <= x)
  }

  /**
   *	Returns 1 if a is greater than or equal to b, 0 otherwise.
 *
   * @param program
   * @return
   */
  def opGreaterThanOrEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_GREATERTHANOREQUAL,
      "Script top must be OP_GREATERTHANOREQUAL")
    performBinaryBooleanOperation(program, (x,y) => y >= x)
  }


  /**
   * Returns the smaller of a and b.
 *
   * @param program
   * @return
   */
  def opMin(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_MIN,
      "Script top must be OP_MIN")

    if (program.stack.size < 2) {
      logger.error("OP_MAX requires at least two stack elements")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      performComparisonOnTwoStackTopItems(program, (x : ScriptNumber, y : ScriptNumber) => if (x < y) x else y)
    }

  }


  /**
   * Returns the larger of a and b.
 *
   * @param program
   * @return
   */
  def opMax(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_MAX,
      "Script top must be OP_MAX")
    if (program.stack.size < 2) {
      logger.error("OP_MAX requires at least two stack elements")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      performComparisonOnTwoStackTopItems(program, (x : ScriptNumber, y : ScriptNumber) => if (x > y) x else y)
    }
  }


  /**
   * Returns 1 if x is within the specified range (left-inclusive), 0 otherwise.
 *
   * @param program
   * @return
   */
  def opWithin(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_WITHIN,
      "Script top must be OP_WITHIN")
    if (program.stack.size < 3) {
      logger.error("OP_WITHIN requires at least 3 elements on the stack")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val c = ScriptNumber(program.stack.head.bytes)
      val b = ScriptNumber(program.stack.tail.head.bytes)
      val a = ScriptNumber(program.stack.tail.tail.head.bytes)

      if (ScriptFlagUtil.requireMinimalData(program.flags) && (!BitcoinScriptUtil.isShortestEncoding(c) ||
        !BitcoinScriptUtil.isShortestEncoding(b) || !BitcoinScriptUtil.isShortestEncoding(a))) {

        logger.error("The constant you gave us is not encoded in the shortest way possible")
        ScriptProgram(program, ScriptErrorMinimalData)
      } else if (isLargerThan4Bytes(c) || isLargerThan4Bytes(b) || isLargerThan4Bytes(a)) {
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error("Cannot perform arithmetic operation on a number larger than 4 bytes, one of these three numbers is larger than 4 bytes: "
          + c + " " + b + " " + a)
        ScriptProgram(program,ScriptErrorUnknownError)
      } else {
        val isWithinRange = a >= b && a < c
        val newStackTop = if (isWithinRange) OP_TRUE else OP_FALSE
        ScriptProgram(program, newStackTop :: program.stack.tail.tail.tail, program.script.tail)
      }


    }

  }


  /**
   * This function checks if a number is <= 4 bytes in size
   * We cannot perform arithmetic operations on bitcoin numbers that are larger than 4 bytes.
   * https://github.com/bitcoin/bitcoin/blob/a6a860796a44a2805a58391a009ba22752f64e32/src/script/script.h#L214-L239
 *
   * @param scriptNumber the script number to be checked
   * @return false if the number is larger than 4 bytes
   */
  private def isLargerThan4Bytes(scriptNumber : ScriptNumber) : Boolean = scriptNumber.bytes.size > 4


  /**
   * Performs the given arithmetic operation on the stack head
 *
   * @param program the program whose stack top is used as an argument for the arithmetic operation
   * @param op the arithmetic ooperation that needs to be executed on the number, for instance incrementing by 1
   * @return the program with the result from performing the arithmetic operation pushed onto the top of the stack
   */
  @tailrec
  private def performUnaryArithmeticOperation(program : ScriptProgram, op : ScriptNumber => ScriptNumber) : ScriptProgram = {
    program.stack.headOption match {
      case None =>
        logger.error("We need one stack element for performing a unary arithmetic operation")
        ScriptProgram(program,ScriptErrorInvalidStackOperation)
      case Some(s : ScriptNumber) =>
        if (ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil.isShortestEncoding(s)) {
          logger.error("The number you gave us is not encoded in the shortest way possible")
          ScriptProgram(program, ScriptErrorMinimalData)
        } else if (isLargerThan4Bytes(s)) {
          logger.error("Cannot perform arithmetic operation on a number larger than 4 bytes, here is the number: " + s)
          //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
          //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
          ScriptProgram(program,ScriptErrorUnknownError)
        } else {
          val newScriptNumber = op(s)
          ScriptProgram(program, newScriptNumber :: program.stack.tail, program.script.tail)
        }
      case Some(s : ScriptConstant) =>
        if (ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil.isShortestEncoding(s)) {
          logger.error("The number you gave us is not encoded in the shortest way possible")
          ScriptProgram(program, ScriptErrorMinimalData)
        } else {
          val interpretedNumber = ScriptNumber(BitcoinSUtil.hexToLong(s.hex))
          val newProgram = ScriptProgram(program, interpretedNumber ::  program.stack.tail, ScriptProgram.Stack)
          performUnaryArithmeticOperation(newProgram, op)
        }

      case Some(s : ScriptToken) =>
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error("Stack top must be a script number/script constant to perform an arithmetic operation")
        ScriptProgram(program,ScriptErrorUnknownError)
    }
  }

  /**
   * Performs the given arithmetic operation on the top two stack items
 *
   * @param program the program whose stack top is used as an argument for the arithmetic operation
   * @param op the arithmetic ooperation that needs to be executed on the number, for instance incrementing by 1
   * @return the program with the result from performing the arithmetic operation pushed onto the top of the stack
   */
  @tailrec
  private def performBinaryArithmeticOperation(program : ScriptProgram, op : (ScriptNumber, ScriptNumber) => ScriptNumber) : ScriptProgram = {
    if (program.stack.size < 2) {
      logger.error("We must have two elements to perform a binary arithmetic operation")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      (program.stack.head, program.stack.tail.head) match {
        case (x : ScriptNumber, y : ScriptNumber) =>
          if (ScriptFlagUtil.requireMinimalData(program.flags) && (!BitcoinScriptUtil.isShortestEncoding(x) || !BitcoinScriptUtil.isShortestEncoding(y))) {
            logger.error("The constant you gave us is not encoded in the shortest way possible")
            ScriptProgram(program, ScriptErrorMinimalData)
          } else if (isLargerThan4Bytes(x) || isLargerThan4Bytes(y)) {
            //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
            //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
            logger.error("Cannot perform arithmetic operation on a number larger than 4 bytes, one of these two numbers is larger than 4 bytes: " + x + " " + y)
            ScriptProgram(program,ScriptErrorUnknownError)
          } else {
            val newStackTop = op(x,y)
            ScriptProgram(program,newStackTop :: program.stack.tail.tail,program.script.tail)
          }
        case (x : ScriptConstant, y : ScriptNumber) =>
          if (ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil.isShortestEncoding(x)) {
            logger.error("The number you gave us is not encoded in the shortest way possible")
            ScriptProgram(program, ScriptErrorMinimalData)
          } else {
            //interpret x as a number
            val interpretedNumber = ScriptNumber(BitcoinSUtil.hexToLong(x.hex))
            val newProgram = ScriptProgram(program, interpretedNumber :: program.stack.tail, ScriptProgram.Stack)
            performBinaryArithmeticOperation(newProgram, op)
          }
        case (x : ScriptNumber, y : ScriptConstant) =>
          if (ScriptFlagUtil.requireMinimalData(program.flags) && !BitcoinScriptUtil.isShortestEncoding(y)) {
            logger.error("The number you gave us is not encoded in the shortest way possible")
            ScriptProgram(program, ScriptErrorMinimalData)
          } else {
          //interpret y as a number
            val interpretedNumber = ScriptNumber(BitcoinSUtil.hexToLong(y.hex))
            val newProgram = ScriptProgram(program, x :: interpretedNumber ::  program.stack.tail, ScriptProgram.Stack)
            performBinaryArithmeticOperation(newProgram, op)
          }
        case (x : ScriptConstant, y : ScriptConstant) =>

          if (ScriptFlagUtil.requireMinimalData(program.flags) && (!BitcoinScriptUtil.isShortestEncoding(x) || !BitcoinScriptUtil.isShortestEncoding(y))) {
            logger.error("The constant you gave us is not encoded in the shortest way possible")
            ScriptProgram(program, ScriptErrorMinimalData)
          } else {
            //interpret x and y as a number
            val interpretedNumberX = ScriptNumber(BitcoinSUtil.hexToLong(x.hex))
            val interpretedNumberY = ScriptNumber(BitcoinSUtil.hexToLong(y.hex))
            val newProgram = ScriptProgram(program, interpretedNumberX :: interpretedNumberY ::  program.stack.tail.tail, ScriptProgram.Stack)
            performBinaryArithmeticOperation(newProgram, op)
          }

        case (x : ScriptToken, y : ScriptToken) =>
          //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
          //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
          logger.error("The top two stack items must be script numbers to perform an arithmetic operation")
          ScriptProgram(program,ScriptErrorUnknownError)
      }
    }


  }

  /**
   * Compares two script numbers with the given boolean operation
 *
   * @param program the program whose two top stack elements are used for the comparison
   * @param op the operation which compares the two script numbers
   * @return the program with either OP_FALSE or OP_TRUE on the stack top
   */
  private def performBinaryBooleanOperation(program : ScriptProgram, op : (ScriptNumber, ScriptNumber) => Boolean) : ScriptProgram = {
    if (program.stack.size < 2) {
      logger.error("We need two stack elements for a binary boolean operation")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val (x,y) = parseTopTwoStackElementsAsScriptNumbers(program)
      if (ScriptFlagUtil.requireMinimalData(program.flags) && (!BitcoinScriptUtil.isShortestEncoding(x) || !BitcoinScriptUtil.isShortestEncoding(y))) {
        logger.error("The constant you gave us is not encoded in the shortest way possible")
        ScriptProgram(program, ScriptErrorMinimalData)
      } else if (isLargerThan4Bytes(x) || isLargerThan4Bytes(y)) {
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error("Cannot perform boolean operation on a number larger than 4 bytes, one of these two numbers is larger than 4 bytes: " + x + " " + y)
        ScriptProgram(program,ScriptErrorUnknownError)
      } else {
        val newStackTop = if(op(x,y)) OP_TRUE else OP_FALSE
        ScriptProgram(program,newStackTop :: program.stack.tail.tail,program.script.tail)
      }
    }
  }


  /**
   * Takes the top two stack items, parses them to numbers then executes the op function on them and places the result
   * onto the stack top
 *
   * @param program the script program whose two top stack items are used as arguments for op
   * @param op the operation that needs to be executed on the two stack top items
   * @return the program with the result of op pushed onto the top of the stack
   */
  private def performComparisonOnTwoStackTopItems(program : ScriptProgram,
    op : (ScriptNumber, ScriptNumber) => ScriptNumber) : ScriptProgram = {
    performBinaryArithmeticOperation(program,op)
  }


  /**
   * Takes the top two stack elements and parses them as script numbers
 *
   * @param program the program whose top two stack elements are being parsed as script numbers
   * @return the tuple with the first element being the first stack element, the second element in the tuple being the second stack element
   */
  private def parseTopTwoStackElementsAsScriptNumbers(program : ScriptProgram) : (ScriptNumber,ScriptNumber) = {
    (program.stack.head, program.stack.tail.head) match {
      case (x : ScriptNumber, y : ScriptNumber) => (x,y)
      case (x : ScriptConstant, y : ScriptNumber) =>
          val interpretedNumber = ScriptNumber(x.hex)
          (interpretedNumber,y)
      case (x : ScriptNumber, y : ScriptConstant) =>
          //interpret y as a number
          val interpretedNumber = ScriptNumber(y.hex)
        (x,interpretedNumber)
      case (x : ScriptConstant, y : ScriptConstant) =>
          //interpret x and y as a number
          val interpretedNumberX = ScriptNumber(x.hex)
          val interpretedNumberY = ScriptNumber(y.hex)
          (interpretedNumberX,interpretedNumberY)

      case (x : ScriptToken, y : ScriptToken) =>
        //pretty sure that an error is thrown inside of CScriptNum which in turn is caught by interpreter.cpp here
        //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L999-L1002
        logger.error("The top two stack items must be script numbers to perform an arithmetic operation")
        throw new RuntimeException("Stack top elements must have be script constants to be interpreted as numbers")
    }
  }
}
