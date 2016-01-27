package org.scalacoin.util

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
}

case class Node[T](v: T, l: BinaryTree[T], r: BinaryTree[T]) extends BinaryTree[T]
case class Leaf[T](v: T) extends BinaryTree[T]
case object Empty extends BinaryTree[Nothing]
