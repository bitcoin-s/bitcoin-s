package org.bitcoins.node.util

trait BitcoinSpvNodeUtil {

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

object BitcoinSpvNodeUtil extends BitcoinSpvNodeUtil
