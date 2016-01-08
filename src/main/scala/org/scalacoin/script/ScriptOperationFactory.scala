package org.scalacoin.script

/**
 * Created by chris on 1/8/16.
 */
trait ScriptOperationFactory[T] {

  /**
   * All of the script operations for a particular T
   * @tparam T
   * @return
   */
  def operations : Seq[T]

  /**
   * Finds a script operation from a given string
   * @param str
   * @return
   */
  def fromString(str : String) : Option[T] = operations.find(_.toString == str)

}
