package org.bitcoins.core

/** This package provides
  * compatability functionality
  * for compiling Scala 2.11, 2.12
  * and 2.13, ideally without leading
  * to any compiler warnings in any of
  * the versions.
  */
package object compat {

  /** Provides imports that allow converting
    * Java collections to Scala collections
    */
  val JavaConverters = scala.jdk.CollectionConverters

}
