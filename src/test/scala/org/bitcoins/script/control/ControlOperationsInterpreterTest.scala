package org.bitcoins.script.control

import org.bitcoins.marshallers.script.ScriptParser
import org.bitcoins.script.error.{ScriptErrorOpReturn, ScriptErrorInvalidStackOperation}
import org.bitcoins.script.{ScriptProgram}
import org.bitcoins.script.arithmetic.OP_ADD
import org.bitcoins.script.bitwise.OP_EQUAL
import org.bitcoins.script.constant._
import org.bitcoins.script.reserved.{OP_VER, OP_RESERVED}
import org.bitcoins.util._
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class ControlOperationsInterpreterTest extends FlatSpec with MustMatchers with ControlOperationsInterpreter {

  "ControlOperationsInterpreter" must "have OP_VERIFY evaluate to true with '1' on the stack" in {
    val stack = List(OP_TRUE)
    val script = List(OP_VERIFY)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val result = opVerify(program)
    result.stack.isEmpty must be (true)
    result.script.isEmpty must be (true)
  }

  it must "have OP_VERIFY evaluate to true when there are multiple items on the stack that can be cast to an int" in {
    //for this test case in bitcoin core's script test suite
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L21
    val stack = ScriptParser.fromString("0x09 0x00000000 0x00000000 0x10")
    val script = List(OP_VERIFY)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val result = opVerify(program)
  }

  it must "have OP_VERIFY evaluate to false with '0' on the stack" in {
    val stack = List(OP_FALSE)
    val script = List(OP_VERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val result = opVerify(program)
    result.stackTopIsFalse must be (true)
  }

  it must "mark the script as invalid for OP_VERIFY when there is nothing on the stack" in {

    val stack = List()
    val script = List(OP_VERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val result = ScriptProgramTestUtil.toExecutedScriptProgram(opVerify(program))
    result.error must be (Some(ScriptErrorInvalidStackOperation))

  }

  it must "fail for verify when there is nothing on the script stack" in {
    intercept[IllegalArgumentException]  {
      val stack = List(ScriptConstantImpl("1"))
      val script = List()
      val program = ScriptProgram(TestUtil.testProgram, stack,script)
      val result = opVerify(program)
    }
  }

  it must "find the first index of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    findFirstOpEndIf(l) must be (Some(0))
    findFirstOpEndIf(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ENDIF)) must be (Some(2))
    findFirstOpEndIf(List(OP_0,OP_1,OP_2)) must be (None)
    findFirstOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)) must be (Some(2))

  }

  it must "find the last index of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    findLastOpEndIf(l) must be (Some(0))
    findLastOpEndIf(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ENDIF)) must be (Some(3))
    findLastOpEndIf(List(OP_0,OP_1,OP_2)) must be (None)
    findLastOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_ENDIF, OP_1)) must be (Some(3))

  }

  it must "find the first indexes of OP_ELSE in a list of script tokens" in {
    findFirstOpElse(List(OP_ELSE)) must be (Some(0))
    findFirstOpElse(List(OP_IF,OP_ELSE,OP_ENDIF,OP_ELSE)) must be (Some(1))
    findFirstOpElse(List(OP_0,OP_1,OP_2)) must be (None)
  }

  it must "find the first indexes of OP_ELSE and OP_ENDIF in a list of script tokens" in {
    findFirstIndexesOpElseOpEndIf(List(OP_ELSE,OP_ENDIF)) must be (Some(0),Some(1))
    findFirstIndexesOpElseOpEndIf(List(OP_IF, OP_ELSE,OP_ENDIF, OP_IF,OP_ELSE,OP_ENDIF)) must be (Some(1),Some(2))
    findFirstIndexesOpElseOpEndIf(List(OP_IF,OP_IF)) must be (None,None)
  }

  it must "remove the first OP_IF expression in a script" in {
    removeFirstOpIf(List(OP_IF,OP_ELSE,OP_ENDIF)) must be (List(OP_ELSE,OP_ENDIF))
    removeFirstOpIf(List(OP_ELSE,OP_ENDIF)) must be (List(OP_ELSE,OP_ENDIF))
    removeFirstOpIf(List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)) must be (List(OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF))
    removeFirstOpIf(List(OP_IF,OP_ENDIF)) must be (List(OP_ENDIF))
  }

  it must "remove the first OP_ELSE expression in a script" in {
    removeFirstOpElse(List(OP_IF,OP_ELSE,OP_ENDIF)) must be (List(OP_IF,OP_ENDIF))
    removeFirstOpElse(List(OP_IF,OP_ENDIF)) must be (List(OP_IF,OP_ENDIF))
    removeFirstOpElse(List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)) must be (List(OP_IF, OP_1, OP_ELSE, OP_3, OP_ENDIF))
  }

  it must "remove the first OP_ELSE in a binary tree" in {
    val script1 = List(OP_IF,OP_ELSE,OP_ENDIF)
    val bTree1 = parseBinaryTree(script1)
    removeFirstOpElse(bTree1).toSeq must be (List(OP_IF))

    val script2 = List(OP_IF,OP_ENDIF)
    val bTree2 = parseBinaryTree(script2)
    removeFirstOpElse(bTree2).toSeq must be (script2)

    val script3 = List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree3 = parseBinaryTree(script3)
    removeFirstOpElse(bTree3).toSeq must be (List(OP_IF, OP_1, OP_ELSE, OP_3, OP_ENDIF))
  }

  it must "find a matching OP_ENDIF for an OP_IF" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val script = List(OP_IF, OP_1, OP_IF, OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ELSE, OP_1, OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    findMatchingOpEndIf(script) must be (20)
  }

  it must "parse a script as a binary tree then convert it back to the original list" in {

    val script0 = List(OP_IF,OP_ENDIF)
    parseBinaryTree(script0).toSeq must be (script0)

    val script1 = List(OP_IF,OP_0,OP_ELSE,OP_1,OP_ENDIF)
    val bTree1 = parseBinaryTree(script1)
    bTree1.toSeq must be (script1)

    val script2 = List(OP_IF,OP_ELSE, OP_ELSE,OP_ENDIF)
    parseBinaryTree(script2).toSeq must be (script2)

    val script3 = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val bTree3 = parseBinaryTree(script3)
    bTree3.toSeq must be (script3)

    val script4 = List(OP_IF, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_IF, OP_2, OP_ELSE, OP_3, OP_ENDIF, OP_ENDIF)
    val bTree4 = parseBinaryTree(script4)

    bTree4.toSeq must be (script4)

    val script5 = List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    parseBinaryTree(script5).toSeq must be (script5)
  }

  it must "parse a script into a binary tree and have the OP_IF expression on the left branch and the OP_ELSE expression on the right branch"in {
    val script = List(OP_IF,OP_0,OP_ELSE,OP_1,OP_ENDIF)
    val bTree = parseBinaryTree(script)
    bTree.value.get must be (OP_IF)

    bTree.left.isDefined must be (true)
    bTree.left.get.value must be (Some(OP_0))

    bTree.right.isDefined must be (true)
    bTree.right.get.value must be (Some(OP_ELSE))

    bTree.right.get.left.isDefined must be (true)
    bTree.right.get.left.get.value must be (Some(OP_1))

    bTree.right.get.right.isDefined must be (true)
    bTree.right.get.left.get.left.get.value must be (Some(OP_ENDIF))
  }

  it must "parse nested OP_ELSE statements into the same branch" in {
    val script = List(OP_IF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree = parseBinaryTree(script)
    bTree.value.get must be (OP_IF)

    bTree.left.isDefined must be (true)
    bTree.left.get.value must be (Some(OP_1))

    bTree.right.isDefined must be (true)
    bTree.right.get.value must be (Some(OP_ELSE))

    bTree.right.get.left.isDefined must be (true)
    bTree.right.get.left.get.value must be (Some(OP_2))

    bTree.right.get.right.isDefined must be (true)
    bTree.right.get.right.get.value must be (Some(OP_ELSE))

    bTree.right.get.right.get.left.isDefined must be (true)
    bTree.right.get.right.get.left.get.value must be (Some(OP_3))

    bTree.right.get.right.get.right.isDefined must be (true)
    bTree.right.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))

    bTree.toSeq must be (script)

  }

  it must "parse a binary tree from a script with nested OP_IFs and OP_ELSES on both branches" in {
    val script = List(OP_IF, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_IF, OP_2, OP_ELSE, OP_3, OP_ENDIF, OP_ENDIF)
    val bTree = parseBinaryTree(script)

    bTree.value must be (Some(OP_IF))

    bTree.left.get.value must be (Some(OP_IF))
    bTree.left.get.right.get.value must be (Some(OP_ELSE))
    bTree.left.get.right.get.left.get.value must be (Some(OP_1))
    bTree.left.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))

    bTree.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.value must be (Some(OP_IF))
    bTree.right.get.left.get.left.get.value must be (Some(OP_2))
    bTree.right.get.left.get.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.right.get.left.get.value must be (Some(OP_3))
    bTree.right.get.left.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))

  }

  it must "parse a binary tree from a script where constants are nested inside of OP_IF OP_ELSE branches" in {
    //"0" "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL"
    val script = List(OP_IF, OP_1,OP_IF,OP_RETURN,OP_ELSE,OP_RETURN,OP_ELSE,OP_RETURN, OP_ENDIF,
      OP_ELSE, OP_1, OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL)
    val bTree = parseBinaryTree(script)
    bTree.toSeq must be (script)
    bTree.right.get.right.get.left.get.left.get.left.get.value must be (Some(OP_ADD))
    bTree.right.get.right.get.left.get.left.get.left.get.left.get.value must be (Some(OP_2))
    bTree.right.get.right.get.left.get.left.get.left.get.left.get.left.get.value must be (Some(OP_EQUAL))
  }

  it must "parse a binary tree where there are nested OP_ELSES in the outer most OP_ELSE" in {
    //https://gist.github.com/Christewart/a5253cf708903323ddc6
    val script = List(OP_IF,OP_1, OP_IF,OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN,OP_ENDIF,
      OP_ELSE, OP_1,OP_IF,OP_1,OP_ELSE,
      OP_RETURN,OP_ELSE,OP_1,OP_ENDIF, OP_ELSE,OP_RETURN,OP_ENDIF,OP_ADD,OP_2,OP_EQUAL)

    val bTree = parseBinaryTree(script)

    bTree.toSeq must be (script)

    bTree.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.value must be (Some(OP_1))

    bTree.right.get.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.right.get.left.get.value must be (Some(OP_RETURN))

    bTree.right.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))
    bTree.right.get.right.get.left.get.left.get.left.get.value must be (Some(OP_ADD))

  }

  it must "parse a binary tree that has OP_NOTIFs" in  {
    val script = List(OP_NOTIF, OP_1,OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree = parseBinaryTree(script)
    bTree.value.get must be (OP_NOTIF)

    bTree.left.isDefined must be (true)
    bTree.left.get.value must be (Some(OP_1))

    bTree.right.isDefined must be (true)
    bTree.right.get.value must be (Some(OP_ELSE))

    bTree.right.get.left.isDefined must be (true)
    bTree.right.get.left.get.value must be (Some(OP_2))

    bTree.right.get.right.isDefined must be (true)
    bTree.right.get.right.get.value must be (Some(OP_ELSE))

    bTree.right.get.right.get.left.isDefined must be (true)
    bTree.right.get.right.get.left.get.value must be (Some(OP_3))

    bTree.right.get.right.get.right.isDefined must be (true)
    bTree.right.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))

    bTree.toSeq must be (script)
  }

  it must "parse a binary tree with nested OP_NOTIFs" in {

    val script = List(OP_NOTIF, OP_NOTIF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_NOTIF, OP_2, OP_ELSE, OP_3, OP_ENDIF, OP_ENDIF)
    val bTree = parseBinaryTree(script)

    bTree.value must be (Some(OP_NOTIF))
    bTree.left.get.value must be (Some(OP_NOTIF))
    bTree.left.get.right.get.value must be (Some(OP_ELSE))
    bTree.left.get.right.get.left.get.value must be (Some(OP_1))
    bTree.left.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))

    bTree.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.value must be (Some(OP_NOTIF))
    bTree.right.get.left.get.left.get.value must be (Some(OP_2))
    bTree.right.get.left.get.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.right.get.left.get.value must be (Some(OP_3))
    bTree.right.get.left.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))

  }



  it must "evaluate an OP_IF correctly" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)
    newProgram.stack.isEmpty must be (true)
    newProgram.script must be (List(OP_ENDIF,OP_1))
  }

  it must "evaluate an OP_IF OP_ELSE OP_ENDIF block" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_VER, OP_ELSE, OP_1, OP_ENDIF)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)
    newProgram.script must be (List(OP_ELSE,OP_1,OP_ENDIF))
  }

  it must "check that every OP_IF has a matching OP_ENDIF" in {
    val script0 = List()
    checkMatchingOpIfOpNotIfOpEndIf(script0) must be (true)

    val script1 = List(OP_IF, OP_ENDIF)
    checkMatchingOpIfOpNotIfOpEndIf(script1) must be (true)

    val script2 = List(OP_IF)
    checkMatchingOpIfOpNotIfOpEndIf(script2) must be (false)

    val script3 = List(OP_IF,OP_IF,OP_NOTIF,OP_ELSE,OP_ELSE,OP_ELSE,OP_ENDIF,OP_ENDIF,OP_ENDIF)
    checkMatchingOpIfOpNotIfOpEndIf(script3) must be (true)
  }

  it must "evaluate an OP_IF block correctly if the stack top is true" in {
    val stack = List(OP_1)
    val script = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)

    newProgram.stack must be (List())
    newProgram.script must be (List(OP_1))
  }

  it must "evaluate a weird case using multiple OP_ELSEs" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_IF, OP_ELSE, OP_0, OP_ELSE, OP_1, OP_ENDIF)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)

    newProgram.script must be (List(OP_ELSE,OP_1,OP_ENDIF))

  }


  it must "evaluate nested OP_IFS correctly" in {
    val stack = List(OP_1)
    val script = List(OP_IF,
      OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF,
      OP_ELSE,
      OP_IF, OP_2, OP_ELSE, OP_3, OP_ENDIF,
      OP_ENDIF)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)

    newProgram.stack.isEmpty must be (true)
    newProgram.script must be (List(OP_IF,OP_0,OP_ELSE,OP_1,OP_ENDIF))
  }

  it must "evaluate a nested OP_IFs OP_ELSES correctly when the stack top is 0" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val stack = List(OP_0)
    //"0", "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE
    // RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL"
    val script = List(OP_IF,OP_1,
      OP_IF,OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN,OP_ENDIF,
      OP_ELSE, OP_1,
      OP_IF,OP_1,OP_ELSE, OP_RETURN,OP_ELSE,OP_1,OP_ENDIF,
      OP_ELSE,OP_RETURN,OP_ENDIF,OP_ADD,OP_2,OP_EQUAL)

    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)

    newProgram.stack.isEmpty must be (true)
    newProgram.script must be (List(OP_ELSE, OP_1,OP_IF,OP_1,OP_ELSE,
      OP_RETURN,OP_ELSE,OP_1,OP_ENDIF,OP_ELSE, OP_RETURN,OP_ENDIF,OP_ADD,OP_2,OP_EQUAL))


    val newProgram1 = opElse(newProgram)
    newProgram1.stack.isEmpty must be (true)
    newProgram1.script must be (List(OP_1,OP_IF,OP_1,OP_ELSE,
      OP_RETURN,OP_ELSE,OP_1,OP_ENDIF,OP_ENDIF,OP_ADD,OP_2,OP_EQUAL))

  }



  it must "remove the first OP_ELSE if the stack top is true for an OP_IF" in  {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL)

    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opIf(program)

    newProgram.stack.isEmpty must be (true)
    newProgram.script must be (List(OP_1,OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL))
  }

  it must "evaluate an OP_ENDIF correctly" in {
    val stack = List(ScriptNumberImpl(1), ScriptNumberImpl(1))
    val script = List(OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opEndIf(program)

    newProgram.stack must be (stack)
    newProgram.script must be (script.tail)
  }


  it must "parse a partial script correctly" in {
    val script = List(OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL)

    val bTree = parseBinaryTree(script)
    bTree.value must be (Some(OP_IF))
    bTree.left.get.value must be (Some(OP_1))
    bTree.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.value must be (Some(OP_RETURN))
    bTree.right.get.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.right.get.left.get.value must be (Some(OP_1))
    bTree.right.get.right.get.left.get.left.get.value must be (Some(OP_ENDIF))
    bTree.right.get.right.get.left.get.left.get.left.get.value must be (Some(OP_ELSE))
  }

  it must "mechanically evaluate this entire script correctly" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_NOTIF, OP_0,
      OP_NOTIF, OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ELSE, OP_0, OP_NOTIF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNotIf(program)

    newProgram.stack.isEmpty must be (true)
    newProgram.script must be (List(OP_ELSE, OP_0, OP_NOTIF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL))

    val newProgram1 = opElse(newProgram)
    newProgram1.stack.isEmpty must be (true)
    newProgram1.script must be (List(OP_0, OP_NOTIF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL))

    val newProgram2 = opNotIf(ScriptProgram(newProgram1,List(OP_0),newProgram1.script.tail))
    newProgram2.stack.isEmpty must be (true)
    newProgram2.script must be (List(OP_1,OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL))

    val newProgram3 = opElse(ScriptProgram(newProgram2,List(OP_1),newProgram2.script.tail))
    newProgram3.stack must be (List(OP_1))
    newProgram3.script must be (List(OP_1,OP_ENDIF,OP_ENDIF,OP_ADD, OP_2, OP_EQUAL))

    val newProgram4 = opEndIf(ScriptProgram(newProgram3, newProgram3.script.head :: newProgram3.stack, newProgram3.script.tail))
    newProgram4.stack must be (List(OP_1,OP_1))
    newProgram4.script must be (List(OP_ENDIF,OP_ADD, OP_2, OP_EQUAL))

    val newProgram5 = opEndIf(newProgram4)
    newProgram5.stack must be (List(OP_1,OP_1))
    newProgram5.script must be (List(OP_ADD, OP_2, OP_EQUAL))

  }

  it must "mark a transaction as invalid if it is trying to spend an OP_RETURN output" in {
    val stack = Seq()
    val script = Seq(OP_RETURN)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opReturn(program))

    newProgram.error must be (Some(ScriptErrorOpReturn))
  }


  it must "remove nothing when trying to remove an OP_ELSE if the tree is empty" in {
    removeFirstOpElse(Empty) must be (Empty)
  }

  it must "remove an OP_ELSE from the left branch from a binary tree if an OP_IF DNE on the left branch" in {
    val tree = Node(OP_0,Node(OP_ELSE,Empty,Empty),Empty)

    removeFirstOpElse(tree) must be (Node(OP_0,Empty,Empty))
  }

  it must "remove the first OP_IF expression a sequence" in {
    val asm = List(OP_IF,OP_0,OP_ELSE,OP_1,OP_ENDIF)
    removeFirstOpIf(asm) must be (Seq(OP_ELSE,OP_1,OP_ENDIF))
  }

}


