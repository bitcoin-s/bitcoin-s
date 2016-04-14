package org.scalacoin.script.arithmetic

import org.scalacoin.script.control.{ControlOperationsInterpreter, OP_VERIFY}
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.util.BitcoinSUtil

/**
 * Created by chris on 1/25/16.
 */
trait ArithmeticInterpreter extends ControlOperationsInterpreter {


  /**
   * a is added to b
   * @param program
   * @return
   */
  def opAdd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ADD, "Script top must be OP_ADD")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_ADD")
    performBinaryArithmeticOperation(program, (x,y) => x + y)
  }

  /**
   * Increments the stack top by 1
   * @param program
   * @return
   */
  def op1Add(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1ADD, "Script top must be OP_1ADD")
    require(program.stack.size > 0, "Must have one item on the stack to execute OP_1ADD")
    performUnaryArithmeticOperation(program, x => x + ScriptNumberFactory.one)
  }

  /**
   * Decrements the stack top by 1
   * @param program
   * @return
   */
  def op1Sub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1SUB, "Script top must be OP_1SUB")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_1SUB")
    performUnaryArithmeticOperation(program, x => x - ScriptNumberFactory.one )
  }


  /**
   * b is subtracted from a.
   * @param program
   * @return
   */
  def opSub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SUB, "Script top must be OP_SUB")
    require(program.stack.size > 1, "Stack must contain two elements to do an OP_SUB")
    performBinaryArithmeticOperation(program, (x,y) => y - x)
  }

  /**
   * Takes the absolute value of the stack top
   * @param program
   * @return
   */
  def opAbs(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ABS, "Script top must be OP_ABS")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_ABS")
    performUnaryArithmeticOperation(program, x => ScriptNumberFactory.fromNumber(x.num.abs))
  }

  /**
   * Negates the stack top
   * @param program
   * @return
   */
  def opNegate(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NEGATE, "Script top must be OP_NEGATE")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_NEGATE")
    performUnaryArithmeticOperation(program, x => x -)
  }

  /**
   * If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
   * @param program
   * @return
   */
  def opNot(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NOT, "Script top must be OP_NOT")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_NOT")
    //TODO: this needs to be modified to have an exhaustive type check
    performUnaryArithmeticOperation(program, x => if (program.stackTopIsFalse) OP_TRUE else OP_FALSE)
  }

  /**
   * Returns 0 if the input is 0. 1 otherwise.
   * @param program
   * @return
   */
  def op0NotEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_0NOTEQUAL, "Script top must be OP_0NOTEQUAL")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_0NOTEQUAL")
    performUnaryArithmeticOperation(program, x => if(x.num == 0) OP_FALSE else OP_TRUE)
  }


  /**
   * 	If both a and b are not 0, the output is 1. Otherwise 0.
   * @param program
   * @return
   */
  def opBoolAnd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLAND, "Script top must be OP_BOOLAND")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_BOOLAND")
    performBinaryBooleanOperation(program,(x,y) => {
      val xIsFalse = (x == ScriptNumberFactory.zero || x == OP_0)
      val yIsFalse = (y == ScriptNumberFactory.zero || y == OP_0)
      if (xIsFalse || yIsFalse) false else true
    })

  }

  /**
   * If a or b is not 0, the output is 1. Otherwise 0.
   * @param program
   * @return
   */
  def opBoolOr(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLOR, "Script top must be OP_BOOLOR")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_BOOLOR")

    performBinaryBooleanOperation(program, (x,y) => {
      if (x == y && (x == ScriptNumberFactory.zero || x == OP_0)) false else true
    })
  }

  /**
   * Returns 1 if the numbers are equal, 0 otherwise.
   * @param program
   * @return
   */
  def opNumEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMEQUAL, "Script top must be OP_NUMEQUAL")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_NUMEQUAL")
    performBinaryBooleanOperation(program,(x,y) => x.numEqual(y))
  }


  /**
   * Same as OP_NUMEQUAL, but runs OP_VERIFY afterward.
   * @param program
   * @return
   */
  def opNumEqualVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMEQUALVERIFY,
      "Script top must be OP_NUMEQUALVERIFY")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_NUMEQUALVERIFY")
    val numEqualProgram = ScriptProgramFactory.factory(program, program.stack, OP_NUMEQUAL :: program.script.tail)
    val numEqualResult = opNumEqual(numEqualProgram)
    val verifyProgram = ScriptProgramFactory.factory(numEqualResult, numEqualResult.stack, OP_VERIFY :: numEqualResult.script)
    val verifyResult = opVerify(verifyProgram)
    verifyResult
  }


  /**
   * 	Returns 1 if the numbers are not equal, 0 otherwise.
   * @param program
   * @return
   */
  def opNumNotEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMNOTEQUAL,
      "Script top must be OP_NUMNOTEQUAL")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_NUMNOTEQUAL")

    performBinaryBooleanOperation(program, (x,y) => {
      x.num != y.num
    })
  }


  /**
   * 	Returns 1 if a is less than b, 0 otherwise.
   * @param program
   * @return
   */
  def opLessThan(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_LESSTHAN,
      "Script top must be OP_LESSTHAN")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_LESSTHAN")
    performBinaryBooleanOperation(program, (x,y) => y < x)
  }


  /**
   * 	Returns 1 if a is greater than b, 0 otherwise.
   * @param program
   * @return
   */
  def opGreaterThan(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_GREATERTHAN,
      "Script top must be OP_GREATERTHAN")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_GREATERTHAN")
    performBinaryBooleanOperation(program, (x,y) => y > x)
  }

  /**
   * Returns 1 if a is less than or equal to b, 0 otherwise.
   * @param program
   * @return
   */
  def opLessThanOrEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_LESSTHANOREQUAL,
      "Script top must be OP_LESSTHANOREQUAL")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_LESSTHANOREQUAL")
    performBinaryBooleanOperation(program, (x,y) => y <= x)
  }

  /**
   *	Returns 1 if a is greater than or equal to b, 0 otherwise.
   * @param program
   * @return
   */
  def opGreaterThanOrEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_GREATERTHANOREQUAL,
      "Script top must be OP_GREATERTHANOREQUAL")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_GREATERTHANOREQUAL")
    performBinaryBooleanOperation(program, (x,y) => y >= x)
  }


  /**
   * Returns the smaller of a and b.
   * @param program
   * @return
   */
  def opMin(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_MIN,
      "Script top must be OP_MIN")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_MIN")
    val b = program.stack.head
    val a = program.stack.tail.head

    val isLessThanOrEqual = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.num <= y.num
      case (x : ScriptNumber, y : ScriptNumberOperation) => x.num <= y.num
      case (x,y) => x.toLong <= y.toLong
    }

    val newStackTop = if (isLessThanOrEqual) a else b
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
  }


  /**
   * Returns the larger of a and b.
   * @param program
   * @return
   */
  def opMax(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_MAX,
      "Script top must be OP_MIN")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_MIN")
    val b = program.stack.head
    val a = program.stack.tail.head

    val isGreaterThanOrEqual = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.num >= y.num
      case (x : ScriptNumber, y : ScriptNumberOperation) => x.num >= y.num
      case (x,y) => x.toLong >= y.toLong
    }

    val newStackTop = if (isGreaterThanOrEqual) a else b
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
  }


  /**
   * Returns 1 if x is within the specified range (left-inclusive), 0 otherwise.
   * @param program
   * @return
   */
  def opWithin(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_WITHIN,
      "Script top must be OP_WITHIN")
    require(program.stack.size > 2, "Stack size must be 3 or more perform an OP_WITHIN")

    val c = program.stack.head
    val b = program.stack.tail.head
    val a = program.stack.tail.tail.head

    val isWithinRange = (a,b,c) match {
      case (x : ScriptNumber, y : ScriptNumber, z : ScriptNumber) => x >= y && x < z
      case (x,y,z) => x.toLong >= y.toLong && x.toLong < z.toLong
    }

    val newStackTop = if (isWithinRange) OP_TRUE else OP_FALSE
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail.tail, program.script.tail)
  }


  /**
   * Converts a script token to an integer
   * @param token
   * @return
   */
  private def numFromScriptToken(token : ScriptToken) : Long = token match {
    case x : ScriptNumber => x.num
    case x : ScriptConstantImpl => Integer.parseInt(x.hex,16)
  }


  /**
   * This function checks if a number is <= 4 bytes in size
   * We cannot perform arithmetic operations on bitcoin numbers that are larger than 4 bytes.
   * https://github.com/bitcoin/bitcoin/blob/a6a860796a44a2805a58391a009ba22752f64e32/src/script/script.h#L214-L239
   * @param scriptNumber the script number to be checked
   * @return if the number is larger than 4 bytes
   */
  private def checkBitcoinIntByteSize(scriptNumber : ScriptNumber) : Boolean = scriptNumber.bytes.size <= 4


  /**
   * Performs the given arithmetic operation on the stack head
   * @param program the program whose stack top is used as an argument for the arithmetic operation
   * @param op the arithmetic ooperation that needs to be executed on the number, for instance incrementing by 1
   * @return the program with the result from performing the arithmetic operation pushed onto the top of the stack
   */
  private def performUnaryArithmeticOperation(program : ScriptProgram, op : ScriptNumber => ScriptNumber) : ScriptProgram = {
    program.stack.head match {
      case s : ScriptNumber =>
        if (checkBitcoinIntByteSize(s)) {
          val newScriptNumber = op(s)
          ScriptProgramFactory.factory(program, newScriptNumber :: program.stack.tail, program.script.tail)
        }
        else {
          logger.error("Cannot perform arithmetic operation on a number larger than 4 bytes, here is the number: " + s)
          ScriptProgramFactory.factory(program,false)
        }
      case s : ScriptToken =>
        logger.error("Stack top must be a script number to perform an arithmetic operation")
        ScriptProgramFactory.factory(program,false)
    }
  }

  /**
   * Performs the given arithmetic operation on the top two stack items
   * @param program the program whose stack top is used as an argument for the arithmetic operation
   * @param op the arithmetic ooperation that needs to be executed on the number, for instance incrementing by 1
   * @return the program with the result from performing the arithmetic operation pushed onto the top of the stack
   */
  private def performBinaryArithmeticOperation(program : ScriptProgram, op : (ScriptNumber, ScriptNumber) => ScriptNumber) : ScriptProgram = {
    (program.stack.head, program.stack.tail.head) match {
      case (x : ScriptNumber, y : ScriptNumber) =>
        if (checkBitcoinIntByteSize(x) && checkBitcoinIntByteSize(y)) {
          val newStackTop = op(x,y)
          ScriptProgramFactory.factory(program,newStackTop :: program.stack.tail.tail,program.script.tail)
        }
        else {
          logger.error("Cannot perform arithmetic operation on a number larger than 4 bytes, one of these two numbers is larger than 4 bytes: " + x + " " + y)
          ScriptProgramFactory.factory(program,false)
        }
      case (x : ScriptConstant, y : ScriptNumber) =>
        //interpret x as a number
        val interpretedNumber = ScriptNumberFactory.fromNumber(BitcoinSUtil.hexToLong(x.hex))
        val newProgram = ScriptProgramFactory.factory(program, interpretedNumber ::  program.stack.tail, ScriptProgramFactory.Stack)
        performBinaryArithmeticOperation(newProgram, op)
      case (x : ScriptNumber, y : ScriptConstant) =>
        val interpretedNumber = ScriptNumberFactory.fromNumber(BitcoinSUtil.hexToLong(y.hex))
        val newProgram = ScriptProgramFactory.factory(program, x :: interpretedNumber ::  program.stack.tail, ScriptProgramFactory.Stack)
        performBinaryArithmeticOperation(newProgram, op)
      case (x : ScriptToken, y : ScriptToken) =>
        logger.error("The top two stack items must be script numbers to perform an arithmetic operation")
        ScriptProgramFactory.factory(program,false)
    }
  }

  /**
   * Compares two script numbers with the given boolean operation
   * @param program the program whose two top stack elements are used for the comparison
   * @param op the operation which compares the two script numbers
   * @return the program with either ScriptFalse or ScriptTrue on the stack top
   */
  private def performBinaryBooleanOperation(program : ScriptProgram, op : (ScriptNumber, ScriptNumber) => Boolean) : ScriptProgram = {
    (program.stack.head, program.stack.tail.head) match {
      case (x : ScriptNumber, y : ScriptNumber) =>
        if (checkBitcoinIntByteSize(x) && checkBitcoinIntByteSize(y)) {
          val newStackTop = if(op(x,y)) OP_TRUE else OP_FALSE
          ScriptProgramFactory.factory(program,newStackTop :: program.stack.tail.tail,program.script.tail)
        }
        else {
          logger.error("Cannot perform boolean operation on a number larger than 4 bytes, one of these two numbers is larger than 4 bytes: " + x + " " + y)
          ScriptProgramFactory.factory(program,false)
        }

      case (x : ScriptToken, y : ScriptToken) =>
        logger.error("The top two stack items must be script numbers to perform an arithmetic operation")
        ScriptProgramFactory.factory(program,false)
    }
  }
}
