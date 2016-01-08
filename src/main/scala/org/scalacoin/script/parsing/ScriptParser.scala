package org.scalacoin.script.parsing

import org.scalacoin.script.ScriptOperation

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
  def parseInputScript(str : String) : List[String] = {
    str.split(" ").toList
  }

/*  def parseOutputScript(str : String) : List[ScriptOperation] = {

  }*/
}
