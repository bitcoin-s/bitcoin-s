package org.bitcoins.node.util

import scala.util.Random

object BitcoinSNodeUtil {

  /** Creates a unique actor name for a actor
    * @param className
    * @return
    */
  def createActorName(className: String): String = {
    // add random number in case we gen actors at the same time
    s"$className-${System.currentTimeMillis()}-${Random.nextLong()}"
  }

  /** Creates a unique actor name for a given class
    * @param className
    * @return
    */
  def createActorName(className: Class[_]): String =
    createActorName(className.getSimpleName)
}
