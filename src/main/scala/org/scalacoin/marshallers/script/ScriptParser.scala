package org.scalacoin.marshallers.script

import org.scalacoin.script._
import org.scalacoin.script.constant._
import org.scalacoin.util.ScalacoinUtil

import scala.annotation.tailrec

/**
 * Created by chris on 1/7/16.
 */
trait ScriptParser extends ScalacoinUtil {

  /**
   * Parses an asm output script of a transaction
   * @param str
   * @return
   */
  def parse(str : String) : List[ScriptToken] = {
/*    require(ScriptOperationFactory.fromString(str.split(" ").head).isDefined,
      "String for parsing was not given in asm format. Needs to have a asm operation, for example OP_DUP")*/
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

    @tailrec
    def loop(bytes : List[Byte], accum : List[ScriptToken]) : List[ScriptToken] = {
      bytes match {
        case h :: t =>
          val op  = ScriptOperationFactory.fromOpCode(h).get
          //means that we need to push x amount of bytes on to the stack
          if (ScriptNumberFactory.operations.contains(op)) {
            val (constant,tail) = pushConstant(ScriptNumberImpl(op.opCode),t)
            loop(tail, constant :: accum)
          } else loop(t, op :: accum)
        case Nil => accum
      }
    }
    loop(bytes, List()).reverse
  }

  private def pushConstant(op : ScriptNumber, bytes : List[Byte]) : (ScriptConstant, List[Byte]) = {
    val finalIndex = op.opCode
    val constant : ScriptConstantImpl = ScriptConstantImpl(encodeHex(bytes.slice(0,finalIndex)))
    (constant, bytes.slice(finalIndex,bytes.size))
  }
}
