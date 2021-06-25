package org.bitcoins.core.util.sorted

import org.bitcoins.core.util.SeqWrapper

/** Wraps a sorted Vector[T] and its Ordering (which may be for a supertype).
  *
  * For example SortedVec[SchnorrNonce, NetworkElement] would be a
  * Vector[SchnorrNonce] sorted as NetworkElements.
  */
abstract class SortedVec[T, B >: T](
    override val wrapped: Vector[T],
    ord: Ordering[B])
    extends SeqWrapper[T] {
  require(
    wrapped.init.zip(wrapped.tail).forall { case (x, y) => ord.lteq(x, y) },
    s"Vector must be sorted. $wrapped")
}

object SortedVec {

  /** For use with ordered lists so that changing order will not be allowed.
    */
  def forOrdered[T](vec: Vector[T]): Ordering[T] = { case (x, y) =>
    vec.indexOf(x) - vec.indexOf(y)
  }

  private case class SortedVecImpl[T, B >: T](vec: Vector[T])(implicit
      val ord: Ordering[B])
      extends SortedVec[T, B](vec, ord)

  def sort[T, B >: T](vec: Vector[T])(implicit
      ord: Ordering[B]): SortedVec[T, B] = {
    SortedVecImpl(vec.sorted[B])
  }

  def apply[T, B >: T](vec: Vector[T])(implicit
      ord: Ordering[B]): SortedVec[T, B] = {
    SortedVecImpl(vec)
  }
}
