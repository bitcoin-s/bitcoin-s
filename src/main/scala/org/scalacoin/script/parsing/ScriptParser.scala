package org.scalacoin.script.parsing

/**
 * Created by chris on 1/7/16.
 */
trait ScriptParser {


  /**
   * Parses a script inside of a transaction 
   * @param str
   * @tparam T
   * @return
   */
  def parse[T](str : String) : List[T] = ???
}
