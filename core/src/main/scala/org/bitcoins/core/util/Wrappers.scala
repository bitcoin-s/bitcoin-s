package org.bitcoins.core.util

trait SeqWrapper[+T] extends IndexedSeq[T] {
  protected def wrapped: IndexedSeq[T]
  override def iterator: Iterator[T] = wrapped.iterator
  override def length: Int = wrapped.length
  override def apply(idx: Int): T = wrapped(idx)
}

trait MapWrapper[K, +T] extends Map[K, T] {
  protected def wrapped: Map[K, T]
  override def +[B1 >: T](kv: (K, B1)): Map[K, B1] = wrapped.+(kv)
  override def get(key: K): Option[T] = wrapped.get(key)
  override def iterator: Iterator[(K, T)] = wrapped.iterator
  override def updated[V1 >: T](key: K, value: V1): Map[K, V1] =
    wrapped.updated(key, value)
}
