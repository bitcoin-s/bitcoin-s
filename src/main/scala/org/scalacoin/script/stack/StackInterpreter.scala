package org.scalacoin.script.stack

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}

/**
 * Created by chris on 1/6/16.
 * Stack operations implemented in the script programming language
 * https://en.bitcoin.it/wiki/Script#Stack
 */
trait StackInterpreter extends BitcoinSLogger {

  /**
   * Duplicates the element on top of the stack
   * expects the first element in script to be the OP_DUP operation
   * @param program
   * @return
   */
  def opDup(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_DUP, "Top of the script stack must be OP_DUP")
    program.stack match {
      case h :: t => ScriptProgramFactory.factory(program, h :: program.stack, program.script.tail)
      case Nil =>
        logger.error("Cannot duplicate the top element on an empty stack")
        ScriptProgramFactory.factory(program,false)
    }
  }

  /**
   * If the top stack value is not 0, duplicate it.
   * @param program
   * @return
   */
  def opIfDup(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_IFDUP, "Top of the script stack must be OP_DUP")
    program.stack.headOption.isDefined match {
      case true if (program.stack.head == OP_0) =>
        ScriptProgramFactory.factory(program,program.stack,program.script.tail)
      case true => ScriptProgramFactory.factory(program, program.stack.head :: program.stack,
        program.script.tail)
      case false =>
        logger.error("Cannot duplicate the top element on an empty stack")
        ScriptProgramFactory.factory(program,false)
    }

  }

  /**
   * Puts the number of stack items onto the stack.
   * @param program
   * @return
   */
  def opDepth(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_DEPTH, "Top of script stack must be OP_DEPTH")
    require(program.script.size >= 1, "OP_DEPTH requires at least two elements on the script stack")
    val stackSize = program.stack.size

    val numberToPush : ScriptNumber= if (stackSize == 0) OP_0 else ScriptNumberImpl(stackSize)
    ScriptProgramFactory.factory(program, numberToPush :: program.stack, program.script.tail)
  }

  /**
   * Puts the input onto the top of the alt stack. Removes it from the main stack.
   * @param program
   * @return
   */
  def opToAltStack(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_TOALTSTACK, "Top of script stack must be OP_TOALTSTACK")
    require(program.stack.size > 0,"Stack must have at least one item on it for OP_TOALTSTACK")
    ScriptProgramFactory.factory(program, program.stack.tail,
      program.script.tail, List(program.stack.head), ScriptProgramFactory.AltStack)
  }

  /**
   * Puts the input onto the top of the main stack. Removes it from the alt stack.
   * @param program
   * @return
   */
  def opFromAltStack(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_FROMALTSTACK, "Top of script stack must be OP_FROMALTSTACK")


    program.altStack.size > 0 match {
      case true => ScriptProgramFactory.factory(program, program.altStack.head :: program.stack,
        program.script.tail, program.altStack.tail, ScriptProgramFactory.AltStack)
      case false =>
        logger.error("Alt Stack must have at least one item on it for OP_FROMALTSTACK")
        ScriptProgramFactory.factory(program,false)
    }

  }

  /**
   * Removes the top stack item.
   * @param program
   * @return
   */
  def opDrop(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_DROP, "Top of script stack must be OP_DROP")
    program.stack.size > 0 match {
      case true => ScriptProgramFactory.factory(program, program.stack.tail,program.script.tail)
      case false =>
        logger.error("Stack must have at least one item on it for OP_DROP")
        ScriptProgramFactory.factory(program,false)
    }

  }


  /**
   * Removes the second-to-top stack item
   * @param program
   * @return
   */
  def opNip(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NIP, "Top of script stack must be OP_NIP")
    program.stack match {
      case h :: _ :: t => ScriptProgramFactory.factory(program, h :: t, program.script.tail)
      case h :: t  =>
        logger.error("Stack must have at least two items on it for OP_NIP")
        ScriptProgramFactory.factory(program,false)
      case Nil =>
        logger.error("Stack must have at least two items on it for OP_NIP")
        ScriptProgramFactory.factory(program,false)
    }
  }


  /**
   * Copies the second-to-top stack item to the top.
   * @param program
   * @return
   */
  def opOver(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_OVER, "Top of script stack must be OP_OVER")
    program.stack match {
      case _ :: h1 :: _ => ScriptProgramFactory.factory(program, h1 :: program.stack, program.script.tail)
      case h :: t => logger.error("Stack must have at least two items on it for OP_OVER")
        ScriptProgramFactory.factory(program,false)
      case Nil =>
        logger.error("Stack must have at least two items on it for OP_OVER")
        ScriptProgramFactory.factory(program,false)
    }
  }

  /**
   * The item n back in the stack is copied to the top.
   * @param program
   * @return
   */
  def opPick(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_PICK, "Top of script stack must be OP_PICK")
    require(program.stack.size > 0,"Stack must have at least two items on it for OP_PICK")

    val n = BitcoinSUtil.hexToLong(program.stack.head.hex).toInt
    //check if n is within the bound of the script
    (n >= 0 && n < program.stack.tail.size) match {
      case true =>
        val newStackTop = program.stack.tail(n)
        ScriptProgramFactory.factory(program,newStackTop :: program.stack.tail, program.script.tail)
      case false =>
        logger.error("The index for OP_PICK would have caused an index out of bounds exception")
        ScriptProgramFactory.factory(program,false)
    }

  }

  /**
   * The item n back in the stack is moved to the top
   * @param program
   * @return
   */
  def opRoll(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ROLL, "Top of script stack must be OP_ROLL")
    require(program.stack.size > 0,"Stack must have at least one items on it for OP_ROLL")
    val n = BitcoinSUtil.hexToLong(program.stack.head.hex).toInt
    (n >= 0 && n < program.stack.tail.size) match {
      case true =>
        val newStackTop = program.stack.tail(n)
        //removes the old instance of the stack top, appends the new index to the head
        val newStack = newStackTop :: program.stack.tail.diff(List(newStackTop))
        ScriptProgramFactory.factory(program,newStack,program.script.tail)
      case false =>
        logger.error("The index for OP_ROLL would have caused an index out of bounds exception")
        ScriptProgramFactory.factory(program,false)
    }
  }

  /**
   * The top three items on the stack are rotated to the left.
   * x1 x2 x3 -> x2 x3 x1
   * @param program
   * @return
   */
  def opRot(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ROT, "Top of script stack must be OP_ROT")

    program.stack match {
      case h :: h1 :: h2 :: t =>
        val newStack = h2 :: h :: h1 :: t
        ScriptProgramFactory.factory(program, newStack,program.script.tail)
      case _ =>
        logger.error("Stack must have at least 3 items on it for OP_ROT")
        ScriptProgramFactory.factory(program,false)
    }

  }

  /**
   * The fifth and sixth items back are moved to the top of the stack.
   * x1 x2 x3 x4 x5 x6 -> x3 x4 x5 x6 x1 x2
   * @param program
   * @return
   */
  def op2Rot(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_2ROT, "Top of script stack must be OP_2ROT")
    val newStack = program.stack match {
      case h :: h1 :: h2 :: h3 :: h4 :: h5 :: t => h4 :: h5 :: h :: h1 :: h2 :: h3 ::  t
      case _ => throw new IllegalArgumentException("Stack must have at least 5 items on it for OP_2ROT")
    }
    ScriptProgramFactory.factory(program, newStack,program.script.tail)
  }

  /**
   * Removes the top two stack items.
   * @param program
   * @return
   */
  def op2Drop(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_2DROP, "Top of script stack must be OP_2DROP")
    require(program.stack.size > 1,"Stack must have at least 2 items on it for OP_2DROP")
    ScriptProgramFactory.factory(program, program.stack.tail.tail, program.script.tail)
  }


  /**
   * The top two items on the stack are swapped.
   * @param program
   * @return
   */
  def opSwap(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SWAP, "Top of script stack must be OP_SWAP")
    program.stack.size > 1 match {
      case true =>
        val newStack = program.stack.tail.head :: program.stack.head :: program.stack.tail.tail
        ScriptProgramFactory.factory(program, newStack, program.script.tail)
      case false =>
        logger.error("Stack must have at least 2 items on it for OP_SWAP")
        ScriptProgramFactory.factory(program,false)
    }

  }


  /**
   * The item at the top of the stack is copied and inserted before the second-to-top item.
   * x1 x2 -> x1 x2 x1
   * @param program
   * @return
   */
  def opTuck(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_TUCK, "Top of script stack must be OP_TUCK")

    program.stack match {
      case h :: h1 :: t =>
        val newStack = h  :: h1 :: h:: t
        ScriptProgramFactory.factory(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 2 items on it for OP_TUCK")
        ScriptProgramFactory.factory(program,false)
    }

  }


  /**
   * Duplicates the top two stack items.
   * @param program
   * @return
   */
  def op2Dup(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_2DUP, "Top of script stack must be OP_2DUP")

    program.stack match {
      case h :: h1 :: t =>
        val newStack = h :: h1 :: h :: h1 :: t
        ScriptProgramFactory.factory(program, newStack, program.script.tail)
      case _ =>
        logger.error("Stack must have at least 2 items on it for OP_2DUP")
        ScriptProgramFactory.factory(program,false)
    }

  }

  /**
   * Duplicates the top three stack items.
   * @param program
   * @return
   */
  def op3Dup(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_3DUP, "Top of script stack must be OP_3DUP")
    program.stack match {
      case h :: h1 :: h2 :: t =>
        val newStack = h :: h1 :: h2 :: h :: h1 :: h2 :: t
        ScriptProgramFactory.factory(program,newStack,program.script.tail)
      case _ =>
        logger.error("Stack must have at least 3 items on it for OP_3DUP")
        ScriptProgramFactory.factory(program,false)
    }

  }


  /**
   * Copies the pair of items two spaces back in the stack to the front.
   * x1 x2 x3 x4 -> x1 x2 x3 x4 x1 x2
   * @param program
   * @return
   */
  def op2Over(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_2OVER, "Top of script stack must be OP_2OVER")

    program.stack match {
      case h :: h1 :: h2 :: h3 :: t =>
        val newStack = h2 :: h3 :: h :: h1 :: h2 :: h3 :: t
        ScriptProgramFactory.factory(program, newStack,program.script.tail)
      case _ =>
        logger.error("Stack must have at least 4 items on it for OP_2OVER")
        ScriptProgramFactory.factory(program,false)
    }

  }

  /**
   * Swaps the top two pairs of items.
   * @param program
   * @return
   */
  def op2Swap(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_2SWAP, "Top of script stack must be OP_2SWAP")

    program.stack match {
      case h :: h1 :: h2 :: h3 :: t  =>
        val newStack = h2 :: h3 :: h :: h1 :: t
        ScriptProgramFactory.factory(program,newStack,program.script.tail)
      case _ =>
        logger.error("Stack must have at least 4 items on it for OP_2SWAP")
        ScriptProgramFactory.factory(program,false)
    }

  }

}
