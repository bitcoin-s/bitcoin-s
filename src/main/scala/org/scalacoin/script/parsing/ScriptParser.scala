package org.scalacoin.script.parsing

import org.scalacoin.script._
import org.scalacoin.script.constant.{OP_0, ScriptConstantImpl, ScriptToken}

import scala.annotation.tailrec

/**
 * Created by chris on 1/7/16.
 */
trait ScriptParser {
  /**
   * Parses an output script of a transaction
   * @param str
   * @return
   */
  def parse(str : String) : List[ScriptToken] = {
    @tailrec
    def loop(operations : List[String], accum : List[ScriptToken]) : List[ScriptToken] = {
      operations match {
        case h :: t if (h == "0") => loop(t, OP_0 :: accum)
        case h :: t if (ScriptOperationFactory.fromString(h).isDefined) =>
          loop(t,ScriptOperationFactory.fromString(h).get :: accum)
        case h :: t => loop(t, ScriptConstantImpl(h) :: accum)
        case Nil => accum
      }
    }
    loop(str.split(" ").toList.reverse, List())
  }


  /**
   * Parses a byte array into a the asm operations for a script
   * will throw an exception if it fails to parse a op code
   * @param bytes
   * @return
   */
  def parse(bytes : List[Byte]) : List[ScriptToken] = {
    val operations : List[ScriptToken] = for {
      byte <- bytes
    } yield ScriptOperationFactory.fromOpCode(byte).get
    operations
  }
}
