package org.bitcoins.core.util

trait SeqWrapper[+T] extends IndexedSeq[T] {
  protected def wrapped: IndexedSeq[T]
  override def iterator: Iterator[T] = wrapped.iterator
  override def length: Int = wrapped.length
  override def apply(idx: Int): T = wrapped(idx)
}
