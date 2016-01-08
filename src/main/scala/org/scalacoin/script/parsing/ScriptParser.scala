package org.scalacoin.script.parsing

import org.scalacoin.script.ScriptOperation

import scala.annotation.tailrec

/**
 * Created by chris on 1/7/16.
 */
trait ScriptParser extends  {


  /**
   * Parses a script inside of a transaction
   * @param str
   * @tparam T
   * @return
   */
  def parseInputScript(str : String) : List[String] = {
    str.split(" ").toList
  }

  /**
   * Parses an output script of a transaction
   * @param str
   * @return
   */
/*  def parseOutputScript(str : String) : List[ScriptOperation] = {

    @tailrec
    def loop(operations : List[String], accum : List[ScriptOperation]) : List[ScriptOperation] = {
      operations match {
        case h :: t if ()
      }

    }

    loop(str.split(" ").toList, List())
  }*/
}
