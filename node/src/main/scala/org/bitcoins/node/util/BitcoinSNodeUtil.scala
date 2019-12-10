package org.bitcoins.node.util

trait BitcoinSNodeUtil {

  /**
    * Creates a unique actor name for a actor
    * @param className
    * @return
    */
  def createActorName(className: String): String = {
    s"${className}-${System.currentTimeMillis()}"
  }

  /**
    * Creates a unique actor name for a given class
    * @param className
    * @return
    */
  def createActorName(className: Class[_]): String =
    createActorName(className.getSimpleName)
}

object BitcoinSNodeUtil extends BitcoinSNodeUtil {

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

}
