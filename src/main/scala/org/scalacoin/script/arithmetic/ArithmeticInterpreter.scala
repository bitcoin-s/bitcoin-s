package org.scalacoin.script.arithmetic

import org.scalacoin.script.control.{ControlOperationsInterpreter, OP_VERIFY}
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgram}
import org.scalacoin.script.constant._

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

    val b  = numFromScriptToken(program.stack.head)
    val a = numFromScriptToken(program.stack(1))

    val result = ScriptNumberFactory.fromNumber(a + b)
    ScriptProgramFactory.factory(program, result :: program.stack.slice(2,program.stack.size),
      program.script.tail)
  }

  /**
   * Increments the stack top by 1
   * @param program
   * @return
   */
  def op1Add(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1ADD, "Script top must be OP_1ADD")
    require(program.stack.size > 0, "Must have one item on the stack to execute OP_1ADD")
    val newStackTop = program.stack.head match {
      case s : ScriptNumber => s + ScriptNumberFactory.one
      case x => throw new IllegalArgumentException("Stack must be script number to perform OP_1ADD, stack top was: " + x)
    }

    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }

  /**
   * Decrements the stack top by 1
   * @param program
   * @return
   */
  def op1Sub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1SUB, "Script top must be OP_1SUB")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_1SUB")

    val newStackTop = program.stack.head match {
      case s : ScriptNumber => s - ScriptNumberFactory.one
      case x => throw new IllegalArgumentException("Stack must be script number to perform OP_1ADD, stack top was: " + x)
    }

    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }


  /**
   * b is subtracted from a.
   * @param program
   * @return
   */
  def opSub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SUB, "Script top must be OP_SUB")
    require(program.stack.size > 1, "Stack must contain two elements to do an OP_SUB")

    val (b,a) = program.stack match {
      case (h : ScriptNumber) :: (h1 : ScriptNumber) :: t => (h,h1)
      case x => throw new IllegalArgumentException("Stack must be script number to perform OP_SUB, stack top was: " + x)

    }
    val newScriptNumber = a - b
    ScriptProgramFactory.factory(program, newScriptNumber :: program.stack.tail, program.script.tail)
  }

  /**
   * Takes the absolute value of the stack top
   * @param program
   * @return
   */
  def opAbs(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ABS, "Script top must be OP_ABS")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_ABS")
    val newStackTop = program.stack.head match {
      case s : ScriptNumber => ScriptNumberFactory.fromNumber(s.num.abs)
      case x => throw new IllegalArgumentException("Stack must be script number to perform OP_ABS, stack top was: " + x)
    }
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }

  /**
   * Negates the stack top
   * @param program
   * @return
   */
  def opNegate(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NEGATE, "Script top must be OP_NEGATE")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_NEGATE")
    val newStackTop = program.stack.head match {
      case s : ScriptNumber => ScriptNumberFactory.fromNumber(-s.num)
      case x => throw new IllegalArgumentException("Stack must be script number to perform OP_ABS, stack top was: " + x)
    }
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
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
    val newStackTop = program.stack.head match {
      case OP_0 => OP_TRUE
      case OP_FALSE => OP_TRUE
      case OP_1 => OP_0
      case ScriptNumberFactory.zero => OP_TRUE
      case ScriptNumberFactory.one => OP_FALSE
      case ScriptFalse => ScriptTrue
      case ScriptTrue => ScriptFalse
      case _ => OP_FALSE
    }
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }

  /**
   * Returns 0 if the input is 0. 1 otherwise.
   * @param program
   * @return
   */
  def op0NotEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_0NOTEQUAL, "Script top must be OP_0NOTEQUAL")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_0NOTEQUAL")
    val newStackTop = program.stack.head match {
      case OP_0 => OP_0
      case ScriptNumberFactory.zero => OP_0
      case _ => OP_1
    }
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }


  /**
   * 	If both a and b are not 0, the output is 1. Otherwise 0.
   * @param program
   * @return
   */
  def opBoolAnd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLAND, "Script top must be OP_BOOLAND")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_BOOLAND")
    val b = program.stack.head
    val a = program.stack.tail.head
    val aIsFalse = (a == ScriptNumberFactory.zero || a == OP_0)
    val bIsFalse = (b == ScriptNumberFactory.zero || b == OP_0)
    val newStackTop = if (aIsFalse || bIsFalse) OP_FALSE else OP_TRUE
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)

  }

  /**
   * If a or b is not 0, the output is 1. Otherwise 0.
   * @param program
   * @return
   */
  def opBoolOr(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLOR, "Script top must be OP_BOOLOR")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_BOOLOR")
    val b = program.stack.head
    val a = program.stack.tail.head
    val newStackTop = if (a == b && (a == ScriptNumberFactory.zero || a == OP_0)) OP_0 else OP_1
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
  }

  /**
   * Returns 1 if the numbers are equal, 0 otherwise.
   * @param program
   * @return
   */
  def opNumEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NUMEQUAL, "Script top must be OP_NUMEQUAL")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_NUMEQUAL")
    val b = program.stack.head
    val a = program.stack.tail.head
    val isSame = (a,b) match {
      case (x : ScriptNumber, y : ScriptNumber) => x.num == y.num
      case (x,y) => x == y
    }

    val newStackTop = if (isSame) OP_TRUE else OP_FALSE
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
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
    val verifyProgram = ScriptProgramFactory.factory(numEqualResult, program.stack, OP_VERIFY :: numEqualResult.script)
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

    val b = program.stack.head
    val a = program.stack.tail.head

    val isSame = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber == y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x == y.scriptNumber
      case (x,y) => x == y
    }

    val newStackTop = if (isSame) OP_0 else OP_1
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
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
    val b = program.stack.head
    val a = program.stack.tail.head

    val isLessThan = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber < y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x < y.scriptNumber
      case (x,y) => x.toLong < y.toLong
    }

    val newStackTop = if (isLessThan) OP_1 else OP_0
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
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
    val b = program.stack.head
    val a = program.stack.tail.head

    val isGreaterThan = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber > y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x > y.scriptNumber
      case (x,y) => x.toLong > y.toLong
    }

    val newStackTop = if (isGreaterThan) OP_1 else OP_0
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
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
    val b = program.stack.head
    val a = program.stack.tail.head

    val isLessThanOrEqual = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber <= y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x <= y.scriptNumber
      case (x,y) => x.toLong <= y.toLong
    }

    val newStackTop = if (isLessThanOrEqual) OP_1 else OP_0
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
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
    val b = program.stack.head
    val a = program.stack.tail.head

    val isGreaterThanOrEqual = (a,b) match {
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber >= y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x >= y.scriptNumber
      case (x,y) => x.toLong >= y.toLong
    }

    val newStackTop = if (isGreaterThanOrEqual) OP_1 else OP_0
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail.tail, program.script.tail)
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
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber <= y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x <= y.scriptNumber
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
      case (x : ScriptNumberOperation, y : ScriptNumber) => x.scriptNumber >= y
      case (x : ScriptNumber, y : ScriptNumberOperation) => x >= y.scriptNumber
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

}
