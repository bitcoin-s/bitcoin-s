package org.bitcoins.core.util

case class Indexed[+T](element: T, index: Int)

object Indexed {

  def apply[T](vec: Vector[T]): Vector[Indexed[T]] = {
    vec.zipWithIndex.map { case (elem, index) => Indexed(elem, index) }
  }

  /** Takes in a given vector of T's with their corresponding index
    * and returns a Vector[Indexed[T]].
    *
    * This is useful in situations where you want to preserve the initial
    * index in a set of elements, but have performed subsequent collection operations (like .filter, .filterNot, .collect etc)
    */
  def fromGivenIndex[T](vec: Vector[(T, Int)]): Vector[Indexed[T]] = {
    vec.map { case (t, idx) => Indexed(t, idx) }
  }
}
