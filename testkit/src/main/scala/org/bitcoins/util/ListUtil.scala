package org.bitcoins.util

object ListUtil {

  /**
    * Generates all unique pairs of elements from `xs`
    */
  def uniquePairs[T](xs: Vector[T]): Vector[(T, T)] =
    for {
      (x, idxX) <- xs.zipWithIndex
      (y, idxY) <- xs.zipWithIndex if idxX < idxY
    } yield (x, y)

  /**
    * Generates a vector of vectors "rotating" the head element
    * over `xs`.
    *
    * {{{
    *   > slideFirst(Vector(1, 2, 3))
    *    Vector(Vector(1, 2, 3), Vector(2, 3, 1), Vector(3, 1, 2))
    * }}}
    */
  def rotateHead[T](xs: Vector[T]): Vector[Vector[T]] = {
    for {
      (x, idxX) <- xs.zipWithIndex
    } yield {
      val (firstHalf, secondHalf) = xs.splitAt(idxX)
      secondHalf ++ firstHalf
    }
  }
}
