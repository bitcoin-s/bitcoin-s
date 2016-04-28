package org.bitcoins.util

import scala.annotation.tailrec
/**
 * Created by chris on 1/27/16.
 */
trait BinaryTree[+T] {
  def value: Option[T] = this match {
    case n: Node[T] => Some(n.v)
    case l: Leaf[T] => Some(l.v)
    case Empty      => None
  }

  def left: Option[BinaryTree[T]] = this match {
    case n: Node[T] => Some(n.l)
    case l: Leaf[T] => None
    case Empty      => None
  }

  def right: Option[BinaryTree[T]] = this match {
    case n: Node[T] => Some(n.r)
    case l: Leaf[T] => None
    case Empty      => None
  }



  /**
   * Creates a sequence with only the leaf values
   * evaluates as depth first from left to right
   * @return
   */
  def toSeqLeafValues : Seq[T] = {
    @tailrec
    def loop(tree : BinaryTree[T],accum : List[T], remainder : List[BinaryTree[T]]) : Seq[T] = tree match {
      case Leaf(x) => if (remainder.isEmpty) x :: accum else loop(remainder.head, x :: accum, remainder.tail)
      case Empty => if (remainder.isEmpty) accum else loop(remainder.head, accum, remainder.tail)
      case Node(_,l,r) => loop(l,accum,r :: remainder)
    }
    loop(this,List(),List()).reverse
  }


  /**
   * A function to find the first occurrence of a predicate inside a binary tree
   * @param t
   * @param tree the default tree is the current instance of the binary tree that the function is being called on
   * @param f the default predicate is equality i.e. if 1 == 1
   * @tparam T
   * @return
   */
  def findFirstDFS[T](t : T)(f : T => Boolean = (x : T) => x == t)(implicit tree : BinaryTree[T] = this) : Option[BinaryTree[T]] = {
    @tailrec
    def loop(subTree : BinaryTree[T], remainder : List[BinaryTree[T]]) : Option[BinaryTree[T]] = {
      subTree match {
        case Empty => if (remainder.isEmpty) None else loop(remainder.head, remainder.tail)
        case Leaf(x) => if (f(x)) Some(Leaf(x)) else if (remainder.isEmpty) None else loop(remainder.head, remainder.tail)
        case Node(v, l, r) =>
          if (f(v)) Some(Node(v,l,r))
          else loop(l, r :: remainder)
      }
    }
    loop(tree,List())
  }


  /**
   * Checks if the binary tree contains a certain element
   * @param t
   * @param tree
   * @param f
   * @tparam T
   * @return
   */
  def contains[T](t : T)(f : T => Boolean = (x : T) => x == t)(implicit tree : BinaryTree[T] = this) = findFirstDFS(t)(f)(tree).isDefined


  def count[T](t : T)(implicit tree : BinaryTree[T] = this) = toSeq.count(_ == t)


  /**
   * Inserts a element into one of the two branches in a binary tree
   * if it cannot insert it because the branches are not empty
   * it throws a runtime exception
   * @param tree
   * @tparam T
   * @return
   */
  def insert[T](t : T)(implicit tree : BinaryTree[T] = this) : BinaryTree[T] = {
    insert(Leaf(t))(tree)
  }

  /**
   * Inserts a tree into one of the two branches in a binary tree
   * if it cannot insert it because the branches are not empty
   * it throws a runtime exception
   * @param subTree
   * @param parentTree
   * @tparam T
   * @return
   */
  def insert[T](subTree : BinaryTree[T])(implicit parentTree : BinaryTree[T]) : BinaryTree[T] = parentTree match {
    case n : Node[T] =>
      if (n.l == Empty) Node[T](n.v,subTree,n.r)
      else if (n.r == Empty) Node[T](n.v,n.l,subTree)
      else throw new RuntimeException("There was no empty branch to insert the new t: " + subTree + "inside of tree: " + parentTree)
    case l : Leaf[T]  => Node(l.v, subTree,Empty)
    case Empty => subTree
  }



  /**
   * Removes the subTree from the parentTree
   * @param subTree - the tree to be removed
   * @param parentTree - the tree of which the @subTree is being removed from
   * @tparam T
   * @return
   */
  def remove[T](subTree : BinaryTree[T])(parentTree : BinaryTree[T] = this) : BinaryTree[T] = {
    //TODO: Optimize into a tail recursive function
    parentTree match {
      case Empty => Empty
      case l : Leaf[T] => if (l == subTree) Empty else l
      case n : Node[T] =>
        if (n == subTree) Empty
        else Node[T](n.v,remove(subTree)(n.l), remove(subTree)(n.r))
    }
  }

  /**
   * Replaces all instances of the original tree with the replacement tree
   * @param originalTree - the tree that needs to be replaced
   * @param replacementTree - the tree that is being put into the original tree
   * @param parentTree - the tree that is being searched for instances to replace
   * @tparam T
   * @return
   */
  def replace[T](originalTree : BinaryTree[T], replacementTree : BinaryTree[T])(implicit parentTree : BinaryTree[T] = this) : BinaryTree[T] = {
    //TODO: Optimize this into a tail recursive function
    parentTree match {
      case Empty => if (originalTree == Empty) replacementTree else Empty
      case l : Leaf[T] => if (l == originalTree) replacementTree else l
      case n : Node[T] => if (n == originalTree) replacementTree else
        Node(n.v,
          replace(originalTree,replacementTree)(n.l),
          replace(originalTree,replacementTree)(n.r))
    }
  }


  def toSeq : Seq[T] = {
    @tailrec
    def loop(tree : BinaryTree[T], accum : List[T], remainder : List[BinaryTree[T]]) : List[T] = tree match {
      case Leaf(x) => if (remainder.isEmpty) accum ++ List(x) else loop(remainder.head,accum ++ List(x),remainder.tail)
      case Empty => if (remainder.isEmpty) accum else loop(remainder.head, accum, remainder.tail)
      case Node(v,l,r) =>
        loop(l,accum ++ List(v), r :: remainder)
    }
    loop(this,List(),List())
  }

  def toList : List[T] = toSeq.toList
}

case class Node[T](v: T, l: BinaryTree[T], r: BinaryTree[T]) extends BinaryTree[T]
case class Leaf[T](v: T) extends BinaryTree[T]
case object Empty extends BinaryTree[Nothing]
