package org.bitcoins.util

import org.bitcoins.script.arithmetic.OP_ADD
import org.bitcoins.script.bitwise.OP_EQUAL
import org.bitcoins.script.constant.{OP_2, ScriptToken, OP_0, OP_1}
import org.bitcoins.script.control.{OP_RETURN, OP_ENDIF, OP_ELSE, OP_IF}
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

  it must "convert a simple binary tree to a sequence" in {
    val bTree = Node(1,Leaf(2), Leaf(3))
    val seq = bTree.toSeq
    seq must be (Seq(1,2,3))

    val bTree1 = Node(1,Node(2,Empty,Empty), Node(3,Empty,Empty))
    val seq1 = bTree1.toSeq
    seq1 must be (List(1,2,3))

    val bTree2 = Node(OP_IF,Node(OP_1,Empty,Empty), Node(OP_ELSE,Node(OP_2,Empty,Empty),Leaf(OP_ENDIF)))
    val seq2 = bTree2.toSeq
    seq2 must be (Seq(OP_IF,OP_1,OP_ELSE, OP_2,OP_ENDIF))
  }


  it must "find the first occurrence of a element in the tree" in {
    val tree = Node[Int](0,Node(1,Leaf(2),Leaf(3)), Node(1,Leaf(4), Leaf(5)))
    val first = tree.findFirstDFS(1)()(tree)

    first must be (Some(Node(1,Leaf(2),Leaf(3))))
  }

  it must "return an empty tree when an element is not found inside of a tree" in {
    val tree = Node[Int](0,Node(1,Leaf(2),Leaf(3)), Node(1,Leaf(4), Leaf(5)))
    val first = tree.findFirstDFS(10)()(tree)

    first must be (None)
  }

  it must "return the first occurrence as just a leaf node if it resides in a leaf" in  {
    val tree = Node[Int](0,Node(1,Leaf(2),Leaf(3)), Node(1,Leaf(4), Leaf(5)))
    val first = tree.findFirstDFS(5)()()
    first must be (Some(Leaf(5)))
  }

  it must "tell if a tree contains a certain element" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))

    tree.contains("Hello")()() must equal (true)
    tree.contains("there")()()  must equal (true)
    tree.contains("1")()()  must equal (true)
    tree.contains("2")()()  must equal (true)
    tree.contains("3")()()  must equal (true)
    tree.contains("4")()()  must equal (true)
  }

  it must "remove a subtree" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    val subTree = Node("there",Leaf("1"),Leaf("2"))
    tree.remove(subTree)() must be (Node("Hello",Empty,Node("3",Empty,Leaf("4"))))
  }

  it must "remove the entire parent tree if given as a subtree" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    tree.remove(tree)() must be (Empty)
  }

  it must "remove no nodes if the given subtree DNE in the parent tree" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    val subTree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("4",Empty,Leaf("4")))

    tree.remove(subTree)() must be (tree)
  }

  it must "replace an entire tree" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    tree.replace(tree,Empty)() must be (Empty)
  }

  it must "replace the left branch of a tree" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    tree.replace(tree.left.get,tree.right.get)() must be (Node[String]("Hello",Node("3",Empty,Leaf("4")),Node("3",Empty,Leaf("4"))))
  }

  it must "replace the right branch of a tree" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    tree.replace(tree.right.get,tree.left.get)() must be (Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("there",Leaf("1"),Leaf("2"))))
  }

  it must "replace nothing in a binary tree if there is no match" in {
    val tree = Node[String]("Hello",Node("there",Leaf("1"),Leaf("2")),Node("3",Empty,Leaf("4")))
    tree.replace(Node("thre",Leaf("1"),Leaf("2")),Empty)() must be (tree)
  }

  it must "insert an element into an empty binary tree" in {
    Empty.insert(1) must be (Leaf(1))
  }

  it must "insert an element into a leaf binary tree" in {
    Leaf(1).insert(2) must be (Node(1,Leaf(2),Empty))
  }

  it must "insert an element into a node binary tree" in {
    Node(1,Empty,Empty).insert(2) must be (Node(1,Leaf(2),Empty))
  }

}
