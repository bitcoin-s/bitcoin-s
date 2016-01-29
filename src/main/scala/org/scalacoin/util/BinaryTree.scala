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
