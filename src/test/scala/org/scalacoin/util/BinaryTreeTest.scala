package org.scalacoin.util

import org.scalacoin.script.constant.{ScriptToken, OP_0, OP_1}
import org.scalacoin.script.control.{OP_ENDIF, OP_ELSE, OP_IF}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/27/16.
 */
class BinaryTreeTest extends FlatSpec with MustMatchers {

  "BinaryTree" must "convert a binary tree to a list with only leaf values" in {
    val bTree = Node(-1,Node(-1,Leaf(0),Leaf(1)),Node(-1,Leaf(2),Leaf(3)))
    bTree.toSeqLeafValues must be (Seq(0,1,2,3))
  }

  it must "convert a binary tree to to a list with node values" in {

    //val script = List(OP_IF, OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF, OP_ELSE, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF)
    val bTree : BinaryTree[ScriptToken] =
      Node[ScriptToken](OP_IF,Node(OP_IF,Leaf(OP_1),Node(OP_ELSE,Leaf(OP_0),Leaf(OP_ENDIF))),
        Node(OP_ELSE,Node(OP_IF,Leaf(OP_0), Node(OP_ELSE,Leaf(OP_1),Leaf(OP_ENDIF))),Leaf(OP_ENDIF)))
    val script = List(OP_IF, OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF, OP_ELSE, OP_IF, OP_0, OP_ELSE, OP_1, OP_ENDIF, OP_ENDIF)
    bTree.toSeq.size must be (script.size)
    bTree.toSeq must be (script)
  }


  it must "find the first occurrence of a element in the tree" in {
    val tree = Node[Int](0,Node(1,Leaf(2),Leaf(3)), Node(1,Leaf(4), Leaf(5)))
    val first = tree.findFirstDFS(1)(tree)()

    first must be (Some(Node(1,Leaf(2),Leaf(3))))
  }

  it must "return an empty tree when an element is not found inside of a tree" in {
    val tree = Node[Int](0,Node(1,Leaf(2),Leaf(3)), Node(1,Leaf(4), Leaf(5)))
    val first = tree.findFirstDFS(10)(tree)()

    first must be (None)
  }

  it must "return the first occurrence as just a leaf node if it resides in a leaf" in  {
    val tree = Node[Int](0,Node(1,Leaf(2),Leaf(3)), Node(1,Leaf(4), Leaf(5)))
    val first = tree.findFirstDFS(5)()()

    first must be (Some(Leaf(5)))

  }
}
