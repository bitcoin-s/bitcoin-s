package org.scalacoin.script.control

import org.scalacoin.script.arithmetic.OP_ADD
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.script.constant._
import org.scalacoin.script.reserved.{OP_VER, OP_RESERVED}
import org.scalacoin.util.Leaf
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class ControlOperationsInterpreterTest extends FlatSpec with MustMatchers with ControlOperationsInterpreter {

  "ControlOperationsInterpreter" must "have OP_VERIFY evaluate to true with '1' on the stack" in {
    val stack = List(ScriptTrue)
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result._3 must be (true)
  }

  it must "have OP_VERIFY evaluate to false with '0' on the stack" in {
    val stack = List(ScriptFalse)
    val script = List(OP_VERIFY)
    val result = verify(stack,script)
    result._3 must be (false)
  }

  it must "fail for OP_VERIFY when there is nothing on the stack" in {
    intercept[IllegalArgumentException]  {
      val stack = List()
      val script = List(OP_VERIFY)
      val result = verify(stack,script)
    }
  }

  it must "fail for verify when there is nothing on the script stack" in {
    intercept[IllegalArgumentException]  {
      val stack = List(ScriptConstantImpl("1"))
      val script = List()
      val result = verify(stack,script)
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

  it must "remvoe the first OP_IF expression in a script" in {
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
    removeFirstOpElse(bTree1).toSeq must be (List(OP_IF,OP_ENDIF))

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


  it must "evaluate an OP_IF correctly" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)
    val (newStack,newScript) = opIf(stack,script)
    newStack.isEmpty must be (true)
    newScript must be (List(OP_ENDIF,OP_1))
  }


  it must "evaluate an OP_IF OP_ELSE OP_ENDIF block" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_VER, OP_ELSE, OP_1, OP_ENDIF)
    val (newStack,newScript) = opIf(stack,script)
    newScript must be (List(OP_ELSE,OP_1,OP_ENDIF))
  }

  it must "check that every OP_IF has a matching OP_ENDIF" in {
    val script0 = List()
    checkMatchingOpIfOpEndIf(script0) must be (true)

    val script1 = List(OP_IF, OP_ENDIF)
    checkMatchingOpIfOpEndIf(script1) must be (true)

    val script2 = List(OP_IF)
    checkMatchingOpIfOpEndIf(script2) must be (false)

    val script3 = List(OP_IF,OP_IF,OP_IF,OP_ELSE,OP_ELSE,OP_ELSE,OP_ENDIF,OP_ENDIF,OP_ENDIF)
    checkMatchingOpIfOpEndIf(script3) must be (true)
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
    bTree.right.get.right.get.value must be (Some(OP_ENDIF))
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
    bTree.right.get.right.get.right.get.value must be (Some(OP_ENDIF))

    bTree.right.get.right.get.left.isDefined must be (true)
    bTree.right.get.right.get.left.get.value must be (Some(OP_3))

    bTree.right.get.right.get.right.isDefined must be (true)
    bTree.right.get.right.get.right must be (Some(Leaf(OP_ENDIF)))

  }

  it must "parse a binary tree from a script with nested OP_IFs and OP_ELSES on both branches" in {
    val script = List(OP_IF, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_IF, OP_2, OP_ELSE, OP_3, OP_ENDIF, OP_ENDIF)
    val bTree = parseBinaryTree(script)

    bTree.value must be (Some(OP_IF))

    bTree.left.get.value must be (Some(OP_IF))
    bTree.left.get.right.get.value must be (Some(OP_ELSE))
    bTree.left.get.right.get.left.get.value must be (Some(OP_1))
    bTree.left.get.right.get.right.get.value must be (Some(OP_ENDIF))

    bTree.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.value must be (Some(OP_IF))
    bTree.right.get.left.get.left.get.value must be (Some(OP_2))
    bTree.right.get.left.get.right.get.value must be (Some(OP_ELSE))
    bTree.right.get.left.get.right.get.left.get.value must be (Some(OP_3))
    bTree.right.get.left.get.right.get.right.get.value must be (Some(OP_ENDIF))

  }

  it must "parse a binary tree from a script where constants are nested inside of OP_IF OP_ELSE branches" in {
    //"0" "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL"
    val script = List(OP_IF, OP_1,OP_IF,OP_RETURN,OP_ELSE,OP_RETURN,OP_ELSE,OP_RETURN, OP_ENDIF,
      OP_ELSE, OP_1, OP_IF, OP_1, OP_ELSE, OP_RETURN, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF,
      OP_ADD, OP_2, OP_EQUAL)
    val bTree = parseBinaryTree(script)
    bTree.toSeq must be (script)

    bTree.right.get.right.get.right.get.left.get.value must be (Some(OP_ADD))
    bTree.right.get.right.get.right.get.left.get.left.get.value must be (Some(OP_2))
    bTree.right.get.right.get.right.get.left.get.left.get.left.get.value must be (Some(OP_EQUAL))
  }

  it must "evaluate an OP_IF block correctly if the stack top is true" in {
    val stack = List(OP_1)
    val script = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val (newStack,newScript) = opIf(stack,script)

    newStack must be (List())
    newScript must be (List(OP_1,OP_ENDIF))
  }

  it must "evaluate a weird case using multiple OP_ELSEs" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_IF, OP_ELSE, OP_0, OP_ELSE, OP_1, OP_ENDIF)

    val (newStack,newScript) = opIf(stack,script)

    newScript must be (List(OP_ELSE,OP_1,OP_ENDIF))

  }
  

  it must "evalute nested OP_IFS correctly" in {
    val stack = List(OP_1,OP_1)
    val script = List(OP_IF, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ELSE, OP_IF, OP_2, OP_ELSE, OP_3, OP_ENDIF, OP_ENDIF)
    val (newStack,newScript) = opIf(stack,script)

    newStack.head must be (OP_1)
    newScript must be (List(OP_IF,OP_0,OP_ELSE,OP_1,OP_ENDIF,OP_ENDIF))
  }

  it must "evaluate a nested OP_IFs OP_ELSES correctly when the stack top is 0" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val stack = List(OP_0)
    //"0", "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE
    // RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL"
    val script = List(OP_IF,OP_1, OP_IF,OP_RETURN, OP_ELSE, OP_RETURN, OP_ELSE, OP_RETURN,OP_ENDIF, OP_ELSE, OP_1,OP_IF,OP_1,OP_ELSE,
      OP_RETURN,OP_ELSE,OP_1,OP_ENDIF, OP_ELSE,OP_RETURN,OP_ENDIF,OP_ADD,OP_2,OP_EQUAL)

    val (newStack,newScript) = opIf(stack,script)

    newStack.isEmpty must be (true)
    newScript must be (List(OP_ELSE, OP_1,OP_IF,OP_1,OP_ELSE,
      OP_RETURN,OP_ELSE,OP_1,OP_ENDIF, OP_ELSE,OP_RETURN,OP_ENDIF,OP_ADD,OP_2,OP_EQUAL))


  }
}


