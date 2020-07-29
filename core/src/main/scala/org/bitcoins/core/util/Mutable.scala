package org.bitcoins.core.util

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
