package org.scalacoin.util

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
    //TODO: Optimize this into a tailrec function
    def loop(tree : BinaryTree[T],accum : List[T]) : Seq[T] = tree match {
      case Leaf(x) => x :: accum
      case Empty => accum
      case Node(_,l,r) => loop(l,List()) ++ loop(r,List())
    }
    loop(this,List())
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
    //TODO: Optimize this to be tail recursive
    if (tree.value.isDefined && f(tree.value.get)) Some(tree)
    else {
      val leftTreeResult : Option[BinaryTree[T]] = if (tree.left.isDefined) findFirstDFS(t)(f)(tree.left.get) else None
      if (leftTreeResult.isDefined) leftTreeResult
      else if (tree.right.isDefined) findFirstDFS(t)(f)(tree.right.get)
      else None
    }
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



  def remove[T](subTree : BinaryTree[T])(parentTree : BinaryTree[T] = this) : BinaryTree[T] = {
    //TODO: Optimize into a tail recursive function
    parentTree match {
      case Empty => Empty
      case l : Leaf[T] => if (l == subTree) Empty else l
      case n : Node[T] => if (n == subTree) Empty
        else Node[T](n.value.get,remove(subTree)(n.left.getOrElse(Empty)),
        remove(subTree)(n.right.getOrElse(Empty)))
    }
  }

  /**
   * Replaces all instances of the original tree with the replacement tree
   * @param originalTree - the tree that needs to be replaced
   * @param replacement - the tree that is being put into the original tree
   * @param parentTree - the tree that is being searched for instances to replace
   * @tparam T
   * @return
   */
  def replace[T](originalTree : BinaryTree[T], replacement : BinaryTree[T])(implicit parentTree : BinaryTree[T] = this) : BinaryTree[T] = {

    parentTree match {
      case Empty => Empty
      case l : Leaf[T] => if (l == originalTree) replacement else l
      case n : Node[T] => if (n == originalTree) replacement else
        Node(n.v,
          replace(originalTree,replacement)(n.l),
          replace(originalTree,replacement)(n.r))
    }
  }



  def toSeq : Seq[T] = {
    //TODO: Optimize this into a tailrec function
    def loop(tree : BinaryTree[T],accum : List[T]) : List[T] = tree match {
      case Leaf(x) => x :: accum
      case Empty => accum
      case Node(v,l,r) => v :: loop(l,List()) ++ loop(r,List())
    }
    loop(this,List())
  }

  def toList : List[T] = toSeq.toList
}

case class Node[T](v: T, l: BinaryTree[T], r: BinaryTree[T]) extends BinaryTree[T]
case class Leaf[T](v: T) extends BinaryTree[T]
case object Empty extends BinaryTree[Nothing]
