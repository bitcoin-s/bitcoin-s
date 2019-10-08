package org.bitcoins.core.script.control

import org.bitcoins.core.script.{
  ExecutionInProgressScriptProgram,
  ScriptProgram
}
import org.bitcoins.core.script.arithmetic.OP_ADD
import org.bitcoins.core.script.bitwise.OP_EQUAL
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.reserved.{OP_RESERVED, OP_VER}
import org.bitcoins.core.script.result.{
  ScriptErrorInvalidStackOperation,
  ScriptErrorOpReturn
}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 1/6/16.
  */
class ControlOperationsInterpreterTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger
  val COI = ControlOperationsInterpreter
  "ControlOperationsInterpreter" must "have OP_VERIFY evaluate to true with '1' on the stack" in {
    val stack = List(OP_TRUE)
    val script = List(OP_VERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val result = COI.opVerify(program)
    result.stack.isEmpty must be(true)
    result.script.isEmpty must be(true)
  }

  it must "have OP_VERIFY evaluate to true when there are multiple items on the stack that can be cast to an int" in {
    //for this test case in bitcoin core's script test suite
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L21
    val stack = ScriptParser.fromString("0x09 0x00000000 0x00000000 0x10")
    val script = List(OP_VERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val result = COI.opVerify(program)
  }

  it must "have OP_VERIFY evaluate to false with '0' on the stack" in {
    val stack = List(OP_FALSE)
    val script = List(OP_VERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val result = COI.opVerify(program)
    result.stackTopIsFalse must be(true)
  }

  it must "mark the script as invalid for OP_VERIFY when there is nothing on the stack" in {

    val stack = List()
    val script = List(OP_VERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val result =
      ScriptProgramTestUtil.toExecutedScriptProgram(COI.opVerify(program))
    result.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "fail for verify when there is nothing on the script stack" in {
    intercept[IllegalArgumentException] {
      val stack = List(ScriptConstant("1"))
      val script = List()
      val program =
        ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
      val result = COI.opVerify(program)
    }
  }

  it must "find the first index of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    COI.findFirstOpEndIf(l) must be(Some(0))
    COI.findFirstOpEndIf(List(OP_IF, OP_ELSE, OP_ENDIF, OP_ENDIF)) must be(
      Some(2))
    COI.findFirstOpEndIf(List(OP_0, OP_1, OP_2)) must be(None)
    COI.findFirstOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)) must be(
      Some(2))

  }

  it must "find the last index of our OP_ENDIF in a list of script tokens" in {
    val l = List(OP_ENDIF)
    COI.findLastOpEndIf(l) must be(Some(0))
    COI.findLastOpEndIf(List(OP_IF, OP_ELSE, OP_ENDIF, OP_ENDIF)) must be(
      Some(3))
    COI.findLastOpEndIf(List(OP_0, OP_1, OP_2)) must be(None)
    COI.findLastOpEndIf(List(OP_IF, OP_RESERVED, OP_ENDIF, OP_ENDIF, OP_1)) must be(
      Some(3))

  }

  it must "find the first indexes of OP_ELSE in a list of script tokens" in {
    COI.findFirstOpElse(List(OP_ELSE)) must be(Some(0))
    COI.findFirstOpElse(List(OP_IF, OP_ELSE, OP_ENDIF, OP_ELSE)) must be(
      Some(1))
    COI.findFirstOpElse(List(OP_0, OP_1, OP_2)) must be(None)
  }

  it must "find the first indexes of OP_ELSE and OP_ENDIF in a list of script tokens" in {
    COI.findFirstIndexesOpElseOpEndIf(List(OP_ELSE, OP_ENDIF)) must be(Some(0),
                                                                       Some(1))
    COI.findFirstIndexesOpElseOpEndIf(
      List(OP_IF, OP_ELSE, OP_ENDIF, OP_IF, OP_ELSE, OP_ENDIF)) must be(Some(1),
                                                                        Some(2))
    COI.findFirstIndexesOpElseOpEndIf(List(OP_IF, OP_IF)) must be(None, None)
  }

  it must "remove the first OP_IF expression in a script" in {
    COI.removeFirstOpIf(List(OP_IF, OP_ELSE, OP_ENDIF)) must be(
      List(OP_ELSE, OP_ENDIF))
    COI.removeFirstOpIf(
      List(OP_IF, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)) must be(
      List(OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF))
    COI.removeFirstOpIf(List(OP_IF, OP_ENDIF)) must be(List(OP_ENDIF))
  }

  it must "remove the first OP_ELSE expression in a script" in {
    COI.removeFirstOpElse(List(OP_IF, OP_ELSE, OP_ENDIF)) must be(
      List(OP_IF, OP_ENDIF))
    COI.removeFirstOpElse(List(OP_IF, OP_ENDIF)) must be(List(OP_IF, OP_ENDIF))
    COI.removeFirstOpElse(
      List(OP_IF, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)) must be(
      List(OP_IF, OP_1, OP_ELSE, OP_3, OP_ENDIF))
  }

  it must "remove the first OP_ELSE in a binary tree" in {
    val script1 = List(OP_IF, OP_ELSE, OP_ENDIF)
    val bTree1 = COI.parseBinaryTree(script1)
    COI.removeFirstOpElse(bTree1).toSeq must be(List(OP_IF, OP_ENDIF))

    val script2 = List(OP_IF, OP_ENDIF)
    val bTree2 = COI.parseBinaryTree(script2)
    bTree2 must be(Node(OP_IF, Empty, Leaf(OP_ENDIF)))
    COI.removeFirstOpElse(bTree2).toSeq must be(script2)

    val script3 = List(OP_IF, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree3 = COI.parseBinaryTree(script3)
    COI.removeFirstOpElse(bTree3).toSeq must be(
      List(OP_IF, OP_1, OP_ELSE, OP_3, OP_ENDIF))
  }

  it must "find a matching OP_ENDIF for an OP_IF" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val script = List(
      OP_IF,
      OP_1,
      OP_IF,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ELSE,
      OP_1,
      OP_IF,
      OP_1,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_1,
      OP_ENDIF,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ADD,
      OP_2,
      OP_EQUAL
    )
    COI.findMatchingOpEndIf(script) must be(20)
  }

  it must "parse a script as a binary tree then convert it back to the original list" in {

    val script0 = List(OP_IF, OP_ENDIF)
    val bTree = COI.parseBinaryTree(script0)
    bTree.right.get must be(Leaf(OP_ENDIF))
    bTree.toSeq must be(script0)

    val script1 = List(OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF)
    val bTree1 = COI.parseBinaryTree(script1)
    bTree1.toSeq must be(script1)

    val script2 = List(OP_IF, OP_ELSE, OP_ELSE, OP_ENDIF)
    COI.parseBinaryTree(script2).toSeq must be(script2)

    val script3 = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val bTree3 = COI.parseBinaryTree(script3)
    bTree3.toSeq must be(script3)

    val script5 = List(OP_IF, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    COI.parseBinaryTree(script5).toSeq must be(script5)

    val script6 = List(OP_IF, OP_IF, OP_1, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF)
    val tree6 = COI.parseBinaryTree(script6)
    tree6.left.get must be(Node(OP_IF, Leaf(OP_1), Leaf(OP_ENDIF)))
    tree6.right.get must be(Node(OP_ELSE, Leaf(OP_2), Leaf(OP_ENDIF)))
    COI.parseBinaryTree(script6).toSeq must be(script6)

    val script7 = List(OP_IF, OP_1, OP_ELSE, OP_IF, OP_2, OP_ENDIF, OP_ENDIF)
    val tree7 = COI.parseBinaryTree(script7)
    val expectedTree7 = Node(
      OP_IF,
      Leaf(OP_1),
      Node(OP_ELSE, Node(OP_IF, Leaf(OP_2), Leaf(OP_ENDIF)), Leaf(OP_ENDIF)))
    tree7 must be(expectedTree7)
    tree7.left must be(expectedTree7.left)
    tree7.right must be(expectedTree7.right)

    val subTree1 =
      Node(OP_IF, Leaf(OP_0), Node(OP_ELSE, Leaf(OP_1), Leaf(OP_ENDIF)))
    val subTree2 =
      Node(OP_ELSE,
           Node(OP_IF, Leaf(OP_2), Node(OP_ELSE, Leaf(OP_3), Leaf(OP_ENDIF))),
           Leaf(OP_ENDIF))
    val expected: BinaryTree[ScriptToken] = Node(OP_IF, subTree1, subTree2)
    val script4 = List(OP_IF,
                       OP_IF,
                       OP_0,
                       OP_ELSE,
                       OP_1,
                       OP_ENDIF,
                       OP_ELSE,
                       OP_IF,
                       OP_2,
                       OP_ELSE,
                       OP_3,
                       OP_ENDIF,
                       OP_ENDIF)
    val bTree4 = COI.parseBinaryTree(script4)
    bTree4.left.get must be(subTree1)
    bTree4.right.get must be(subTree2)
    bTree4 must be(expected)
    logger.debug("bTree4: " + bTree4)
    bTree4.toSeq must be(script4)

    val script8 = List(OP_IF, OP_0, OP_ENDIF, OP_2)
    val tree8 = COI.parseBinaryTree(script8)
    tree8.toSeq must be(script8)

  }
  it must "parse a script into a binary tree and have the OP_IF expression on the left branch and the OP_ELSE expression on the right branch" in {
    val script = List(OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF)
    val bTree = COI.parseBinaryTree(script)
    bTree.value.get must be(OP_IF)

    bTree.left.isDefined must be(true)
    bTree.left.get.value must be(Some(OP_0))

    bTree.right.isDefined must be(true)
    bTree.right.get.value must be(Some(OP_ELSE))

    bTree.right.get.left.isDefined must be(true)
    bTree.right.get.left.get.value must be(Some(OP_1))

    bTree.right.get.right.isDefined must be(true)
    bTree.right.get.right.get.value must be(Some(OP_ENDIF))
  }

  it must "parse nested OP_ELSE statements into the same branch" in {
    val script = List(OP_IF, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree = COI.parseBinaryTree(script)
    bTree.value.get must be(OP_IF)

    bTree.left.isDefined must be(true)
    bTree.left.get.value must be(Some(OP_1))

    bTree.right.isDefined must be(true)
    bTree.right.get.value must be(Some(OP_ELSE))

    bTree.right.get.left.isDefined must be(true)
    bTree.right.get.left.get.value must be(Some(OP_2))

    bTree.right.get.right.isDefined must be(true)
    bTree.right.get.right.get.value must be(Some(OP_ELSE))

    bTree.right.get.right.get.left.isDefined must be(true)
    bTree.right.get.right.get.left.get.value must be(Some(OP_3))

    bTree.right.get.right.get.right.isDefined must be(true)
    bTree.right.get.right.get.right.get.value must be(Some(OP_ENDIF))

    bTree.toSeq must be(script)

  }

  it must "parse a binary tree from a script with nested OP_IFs and OP_ELSES on both branches" in {
    val script = List(OP_IF,
                      OP_IF,
                      OP_0,
                      OP_ELSE,
                      OP_1,
                      OP_ENDIF,
                      OP_ELSE,
                      OP_IF,
                      OP_2,
                      OP_ELSE,
                      OP_3,
                      OP_ENDIF,
                      OP_ENDIF)
    val bTree = COI.parseBinaryTree(script)

    bTree.value must be(Some(OP_IF))

    bTree.left.get.value must be(Some(OP_IF))
    bTree.left.get.right.get.value must be(Some(OP_ELSE))
    bTree.left.get.right.get.left.get.value must be(Some(OP_1))
    bTree.left.get.right.get.right.get.value must be(Some(OP_ENDIF))

    bTree.right.get.value must be(Some(OP_ELSE))
    bTree.right.get.left.get.value must be(Some(OP_IF))
    bTree.right.get.left.get.left.get.value must be(Some(OP_2))
    bTree.right.get.left.get.right.get.value must be(Some(OP_ELSE))
    bTree.right.get.left.get.right.get.left.get.value must be(Some(OP_3))
    bTree.right.get.left.get.right.get.right.get.value must be(Some(OP_ENDIF))

  }

  it must "parse a binary tree from a script where constants are nested inside of OP_IF OP_ELSE branches" in {
    val script =
      List(OP_IF, OP_0, OP_IF, OP_1, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF)
    val bTree = COI.parseBinaryTree(script)
    bTree.left.get.value.get must be(OP_0)
    bTree.left.get.left.get.value.get must be(OP_IF)
    bTree.toSeq must be(script)

    val script1 =
      List(OP_IF, OP_1, OP_ELSE, OP_2, OP_IF, OP_3, OP_ENDIF, OP_ENDIF)
    val bTree1 = COI.parseBinaryTree(script1)
    bTree1.toSeq must be(script1)

    val script2 =
      List(OP_IF, OP_0, OP_ELSE, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree2 = COI.parseBinaryTree(script2)
    bTree2.left.get must be(Leaf(OP_0))
    bTree2.right.get must be(
      Node(
        OP_ELSE,
        Leaf(OP_1),
        Node(OP_ELSE, Leaf(OP_2), Node(OP_ELSE, Leaf(OP_3), Leaf(OP_ENDIF)))))
    bTree2.toSeq must be(script2)

    val script3 =
      List(OP_IF, OP_0, OP_ELSE, OP_IF, OP_1, OP_ENDIF, OP_ELSE, OP_2, OP_ENDIF)
    val bTree3 = COI.parseBinaryTree(script3)
    bTree3.toSeq must be(script3)
    //"0" "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL"
    val script4 = List(
      OP_IF,
      OP_0,
      OP_IF,
      OP_1,
      OP_ELSE,
      OP_2,
      OP_ELSE,
      OP_3,
      OP_ENDIF,
      OP_ELSE,
      OP_4,
      OP_IF,
      OP_5,
      OP_ELSE,
      OP_6,
      OP_ELSE,
      OP_7,
      OP_ENDIF,
      OP_ELSE,
      OP_8,
      OP_ENDIF,
      OP_ADD,
      OP_9,
      OP_EQUAL
    )

    val subTree40 =
      Node(OP_IF,
           Leaf(OP_1),
           Node(OP_ELSE, Leaf(OP_2), Node(OP_ELSE, Leaf(OP_3), Leaf(OP_ENDIF))))
    val subTree41 = Node(OP_0, subTree40, Empty)

    val subTree42 =
      Node(OP_IF,
           Leaf(OP_5),
           Node(OP_ELSE, Leaf(OP_6), Node(OP_ELSE, Leaf(OP_7), Leaf(OP_ENDIF))))

    val subTree43 = Node(OP_ADD, Node(OP_9, Leaf(OP_EQUAL), Empty), Empty)
    val subTree44 = Node(OP_ELSE, Leaf(OP_8), Node(OP_ENDIF, subTree43, Empty))
    val subTree45 = Node(OP_ELSE, Node(OP_4, subTree42, Empty), subTree44)

    val expectedBTree4 = Node(OP_IF, subTree41, subTree45)
    val bTree4 = COI.parseBinaryTree(script4)
    bTree4.left.get must be(subTree41)
    bTree4.right.get.left.get must be(subTree45.l)
    bTree4.right.get.right.get must be(subTree45.r)
    bTree4 must be(expectedBTree4)
    bTree4.toSeq must be(script4)
    bTree4.right.get.right.get.right.get.left.get.value must be(Some(OP_ADD))
    bTree4.right.get.right.get.right.get.left.get.left.get.value must be(
      Some(OP_9))
    bTree4.right.get.right.get.right.get.left.get.left.get.left.get.value must be(
      Some(OP_EQUAL))
  }

  it must "parse a binary tree where there are nested OP_ELSES in the outer most OP_ELSE" in {
    //https://gist.github.com/Christewart/a5253cf708903323ddc6
    val script = List(
      OP_IF,
      OP_1,
      OP_IF,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ELSE,
      OP_1,
      OP_IF,
      OP_1,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_1,
      OP_ENDIF,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ADD,
      OP_2,
      OP_EQUAL
    )

    val bTree = COI.parseBinaryTree(script)

    bTree.toSeq must be(script)

    bTree.right.get.value must be(Some(OP_ELSE))
    bTree.right.get.left.get.value must be(Some(OP_1))

    bTree.right.get.right.get.value must be(Some(OP_ELSE))
    bTree.right.get.right.get.left.get.value must be(Some(OP_RETURN))

    bTree.right.get.right.get.right.get.value must be(Some(OP_ENDIF))
    bTree.right.get.right.get.right.get.left.get.value must be(Some(OP_ADD))

  }

  it must "parse a binary tree that has OP_NOTIFs" in {
    val script = List(OP_NOTIF, OP_1, OP_ELSE, OP_2, OP_ELSE, OP_3, OP_ENDIF)
    val bTree = COI.parseBinaryTree(script)
    bTree.value.get must be(OP_NOTIF)

    bTree.left.get.value must be(Some(OP_1))

    bTree.right.get.value must be(Some(OP_ELSE))

    bTree.right.get.left.get.value must be(Some(OP_2))

    bTree.right.get.right.get.value must be(Some(OP_ELSE))

    bTree.right.get.right.get.left.get.value must be(Some(OP_3))

    bTree.right.get.right.get.right.get.value must be(Some(OP_ENDIF))

    bTree.toSeq must be(script)
  }

  it must "parse a binary tree with nested OP_NOTIFs" in {

    val script = List(OP_NOTIF,
                      OP_NOTIF,
                      OP_0,
                      OP_ELSE,
                      OP_1,
                      OP_ENDIF,
                      OP_ELSE,
                      OP_NOTIF,
                      OP_2,
                      OP_ELSE,
                      OP_3,
                      OP_ENDIF,
                      OP_ENDIF)
    val bTree = COI.parseBinaryTree(script)

    bTree.value must be(Some(OP_NOTIF))
    bTree.left.get.value must be(Some(OP_NOTIF))
    bTree.left.get.right.get.value must be(Some(OP_ELSE))
    bTree.left.get.right.get.left.get.value must be(Some(OP_1))
    bTree.left.get.right.get.right.get.value must be(Some(OP_ENDIF))

    bTree.right.get.value must be(Some(OP_ELSE))
    bTree.right.get.left.get.value must be(Some(OP_NOTIF))
    bTree.right.get.left.get.left.get.value must be(Some(OP_2))
    bTree.right.get.left.get.right.get.value must be(Some(OP_ELSE))
    bTree.right.get.left.get.right.get.left.get.value must be(Some(OP_3))
    bTree.right.get.left.get.right.get.right.get.value must be(Some(OP_ENDIF))

  }

  it must "evaluate an OP_IF correctly" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)
    newProgram.stack.isEmpty must be(true)
    newProgram.script must be(List(OP_ENDIF, OP_1))
  }

  it must "evaluate an OP_IF OP_ELSE OP_ENDIF block" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_VER, OP_ELSE, OP_1, OP_ENDIF)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)
    newProgram.script must be(List(OP_ELSE, OP_1, OP_ENDIF))
  }

  it must "check that every OP_IF has a matching OP_ENDIF" in {
    val script0 = List()
    COI.checkMatchingOpIfOpNotIfOpEndIf(script0) must be(true)

    val script1 = List(OP_IF, OP_ENDIF)
    COI.checkMatchingOpIfOpNotIfOpEndIf(script1) must be(true)

    val script2 = List(OP_IF)
    COI.checkMatchingOpIfOpNotIfOpEndIf(script2) must be(false)

    val script3 = List(OP_IF,
                       OP_IF,
                       OP_NOTIF,
                       OP_ELSE,
                       OP_ELSE,
                       OP_ELSE,
                       OP_ENDIF,
                       OP_ENDIF,
                       OP_ENDIF)
    COI.checkMatchingOpIfOpNotIfOpEndIf(script3) must be(true)
  }

  it must "evaluate an OP_IF block correctly if the stack top is true" in {
    val stack = List(OP_1)
    val script = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)

    newProgram.stack must be(Nil)
    newProgram.script must be(List(OP_1, OP_ENDIF))
  }

  it must "evalute an OP_IF block with and leave the remaining operations outside of the OP_IF" in {
    val stack = List(OP_TRUE)
    val script = List(OP_IF, OP_ELSE, OP_ENDIF, OP_1)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)
    newProgram.stack.isEmpty must be(true)
    newProgram.script must be(List(OP_ENDIF, OP_1))
  }

  it must "evaluate a weird case using multiple OP_ELSEs" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_IF, OP_ELSE, OP_0, OP_ELSE, OP_1, OP_ENDIF)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)

    newProgram.script must be(List(OP_ELSE, OP_1, OP_ENDIF))

  }

  it must "evaluate nested OP_IFS correctly" in {
    val stack = List(OP_1)
    val script = List(OP_IF,
                      OP_IF,
                      OP_0,
                      OP_ELSE,
                      OP_1,
                      OP_ENDIF,
                      OP_ELSE,
                      OP_IF,
                      OP_2,
                      OP_ELSE,
                      OP_3,
                      OP_ENDIF,
                      OP_ENDIF)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)

    newProgram.stack.isEmpty must be(true)
    newProgram.script must be(
      List(OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF))
  }

  it must "evaluate a nested OP_IFs OP_ELSES correctly when the stack top is 0" in {
    //https://gist.github.com/Christewart/381dc1dbbb07e62501c3
    val stack = List(OP_0)
    //"0", "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE
    // RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL"
    val script = List(
      OP_IF,
      OP_1,
      OP_IF,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ELSE,
      OP_1,
      OP_IF,
      OP_1,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_1,
      OP_ENDIF,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ADD,
      OP_2,
      OP_EQUAL
    )

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)

    newProgram.stack.isEmpty must be(true)
    newProgram.script must be(
      List(OP_ELSE,
           OP_1,
           OP_IF,
           OP_1,
           OP_ELSE,
           OP_RETURN,
           OP_ELSE,
           OP_1,
           OP_ENDIF,
           OP_ELSE,
           OP_RETURN,
           OP_ENDIF,
           OP_ADD,
           OP_2,
           OP_EQUAL))

    newProgram.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)

    val newProgram1 =
      COI.opElse(newProgram.asInstanceOf[ExecutionInProgressScriptProgram])
    newProgram1.stack.isEmpty must be(true)
    newProgram1.script must be(
      List(OP_1,
           OP_IF,
           OP_1,
           OP_ELSE,
           OP_RETURN,
           OP_ELSE,
           OP_1,
           OP_ENDIF,
           OP_ENDIF,
           OP_ADD,
           OP_2,
           OP_EQUAL))

  }

  it must "remove the first OP_ELSE if the stack top is true for an OP_IF" in {
    val stack = List(ScriptNumber(1))
    val script = List(OP_IF,
                      OP_1,
                      OP_ELSE,
                      OP_RETURN,
                      OP_ELSE,
                      OP_1,
                      OP_ENDIF,
                      OP_ELSE,
                      OP_RETURN,
                      OP_ENDIF,
                      OP_ADD,
                      OP_2,
                      OP_EQUAL)

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opIf(program)

    newProgram.stack.isEmpty must be(true)
    newProgram.script must be(
      List(OP_1,
           OP_ELSE,
           OP_1,
           OP_ENDIF,
           OP_ELSE,
           OP_RETURN,
           OP_ENDIF,
           OP_ADD,
           OP_2,
           OP_EQUAL))
  }

  it must "evaluate an OP_ENDIF correctly" in {
    val stack = List(ScriptNumber(1), ScriptNumber(1))
    val script =
      List(OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opEndIf(program)

    newProgram.stack must be(stack)
    newProgram.script must be(script.tail)
  }

  it must "parse a partially executed script correctly" in {
    val script = List(OP_ENDIF, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    val bTree = COI.parseBinaryTree(script)
    bTree must be(
      Node(OP_ENDIF,
           Empty,
           Node(OP_ENDIF,
                Node(OP_ADD, Node(OP_2, Leaf(OP_EQUAL), Empty), Empty),
                Empty)))
    bTree.toSeq must be(script)
  }

  it must "mechanically evaluate this entire script correctly" in {
    val stack = List(ScriptNumber.one)
    val script = List(
      OP_NOTIF,
      OP_0,
      OP_NOTIF,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ELSE,
      OP_0,
      OP_NOTIF,
      OP_1,
      OP_ELSE,
      OP_RETURN,
      OP_ELSE,
      OP_1,
      OP_ENDIF,
      OP_ELSE,
      OP_RETURN,
      OP_ENDIF,
      OP_ADD,
      OP_2,
      OP_EQUAL
    )

    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = COI.opNotIf(program)

    newProgram.stack.isEmpty must be(true)
    newProgram.script must be(
      List(OP_ELSE,
           OP_0,
           OP_NOTIF,
           OP_1,
           OP_ELSE,
           OP_RETURN,
           OP_ELSE,
           OP_1,
           OP_ENDIF,
           OP_ELSE,
           OP_RETURN,
           OP_ENDIF,
           OP_ADD,
           OP_2,
           OP_EQUAL))

    newProgram.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)

    val newProgram1 =
      COI.opElse(newProgram.asInstanceOf[ExecutionInProgressScriptProgram])
    newProgram1.stack.isEmpty must be(true)
    newProgram1.script must be(
      List(OP_0,
           OP_NOTIF,
           OP_1,
           OP_ELSE,
           OP_RETURN,
           OP_ELSE,
           OP_1,
           OP_ENDIF,
           OP_ENDIF,
           OP_ADD,
           OP_2,
           OP_EQUAL))

    newProgram1.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)

    logger.info("newProgram1.script.tail: " + newProgram1.script.tail)
    val tree = COI.parseBinaryTree(newProgram1.script.tail)

    tree.toSeq must be(newProgram1.script.tail)
    val newProgram2 = COI.opNotIf(
      ScriptProgram(newProgram1.asInstanceOf[ExecutionInProgressScriptProgram],
                    List(OP_0),
                    newProgram1.script.tail))
    newProgram2.stack.isEmpty must be(true)
    newProgram2.script must be(
      List(OP_1, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL))

    newProgram2.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)

    val newProgram3 = COI.opElse(
      ScriptProgram(newProgram2.asInstanceOf[ExecutionInProgressScriptProgram],
                    List(OP_1),
                    newProgram2.script.tail))
    newProgram3.stack must be(List(OP_1))
    newProgram3.script must be(
      List(OP_1, OP_ENDIF, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL))

    newProgram3.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)

    val newProgram4 = COI.opEndIf(
      ScriptProgram(newProgram3.asInstanceOf[ExecutionInProgressScriptProgram],
                    newProgram3.script.head :: newProgram3.stack,
                    newProgram3.script.tail))
    newProgram4.stack must be(List(OP_1, OP_1))
    newProgram4.script must be(List(OP_ENDIF, OP_ADD, OP_2, OP_EQUAL))

    newProgram4.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)

    val newProgram5 =
      COI.opEndIf(newProgram4.asInstanceOf[ExecutionInProgressScriptProgram])
    newProgram5.stack must be(List(OP_1, OP_1))
    newProgram5.script must be(List(OP_ADD, OP_2, OP_EQUAL))

    newProgram5.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)
  }

  it must "mark a transaction as invalid if it is trying to spend an OP_RETURN output" in {
    val stack = Seq()
    val script = Seq(OP_RETURN)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(COI.opReturn(program))

    newProgram.error must be(Some(ScriptErrorOpReturn))
  }

  it must "remove nothing when trying to remove an OP_ELSE if the tree is empty" in {
    COI.removeFirstOpElse(Empty) must be(Empty)
  }

  it must "remove an OP_ELSE from the left branch from a binary tree if an OP_IF DNE on the left branch" in {
    val tree = Node(OP_0, Empty, Node(OP_ELSE, Empty, Empty))

    COI.removeFirstOpElse(tree) must be(Node(OP_0, Empty, Empty))
  }

  it must "remove the first OP_IF expression a sequence" in {
    val asm = List(OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF)
    COI.removeFirstOpIf(asm) must be(Seq(OP_ELSE, OP_1, OP_ENDIF))
  }
}
