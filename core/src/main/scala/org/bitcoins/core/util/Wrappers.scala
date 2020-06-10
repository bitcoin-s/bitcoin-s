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

class Mutable[A](initialValue: A) {
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock()

  private var value: A = initialValue

  def atomicGet: A = {
    lock.readLock().lock()
    try value
    finally lock.readLock().unlock()
  }

  def atomicSet(f: => A): Unit = {
    lock.writeLock().lock()
    try value = f
    finally lock.writeLock().unlock()
  }

  def atomicUpdate[B](b: B)(update: (A, B) => A): A = {
    lock.writeLock().lock()
    try {
      value = update(value, b)
      value
    } finally lock.writeLock().unlock()
  }
}
