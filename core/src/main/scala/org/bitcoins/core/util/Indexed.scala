package org.bitcoins.core.util

case class Indexed[T](element: T, index: Int)

object Indexed {

  def apply[T](vec: Vector[T]): Vector[Indexed[T]] = {
    vec.zipWithIndex.map { case (elem, index) => Indexed(elem, index) }
  }
}
