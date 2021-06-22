package org.bitcoins.core.util

import org.bitcoins.crypto.NetworkElement

/** Wraps a sorted Vector[T] and its Ordering (which may be for a supertype).
  *
  * For example SortedVec[SchnorrNonce, NetworkElement] would be a
  * Vector[SchnorrNonce] sorted as NetworkElements.
  *
  * In the case that you are using a SortedVec[T, T], you may use the type
  * alias SortedVector[T]
  */
case class SortedVec[T, B >: T](vec: Vector[T])(implicit ord: Ordering[B])
    extends SeqWrapper[T] {
  require(vec.init.zip(vec.tail).forall { case (x, y) => ord.lteq(x, y) },
          s"Vector must be sorted. $vec")

  override protected def wrapped: Vector[T] = vec
}

object SortedVec {

  /** A type alias for the common pattern where T is sorted directly.
    * E.g. SortedVector[Int].
    */
  type SortedVector[T] = SortedVec[T, T]

  implicit val networkElementOrd: Ordering[NetworkElement] = {
    case (x: NetworkElement, y: NetworkElement) =>
      x.bytes.compare(y.bytes)
  }

  def sort[T, B >: T](vec: Vector[T])(implicit
      ord: Ordering[B]): SortedVec[T, B] = {
    SortedVec(vec.sorted[B])
  }
}
