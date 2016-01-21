package org.scalacoin.marshallers.script

import org.scalacoin.script._
import org.scalacoin.script.constant._
import org.scalacoin.util.ScalacoinUtil
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/7/16.
 */
trait ScriptParser extends ScalacoinUtil {

  private def logger = LoggerFactory.getLogger(this.getClass)
  /**
   * Parses an asm output script of a transaction
   * example: "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"
   * @param str
   * @return
   */
  def parse(str : String) : List[ScriptToken] = {
/*    require(ScriptOperationFactory.fromString(str.split(" ").head).isDefined,
      "String for parsing was not given in stringified asm format.\n" +
        "Needs to have a asm operation, for example " +
        "\"OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG\"\n" +
        "given string was: " + str)*/
    @tailrec
    def loop(operations : List[String], accum : List[ScriptToken]) : List[ScriptToken] = {
      logger.debug("Attempting to parse: " + operations.headOption)
      operations match {
        //if we see a byte constant in the form of "0x09"
        case h  :: t if (h.size > 1 && h.substring(0,2) == "0x") => loop(t,parseBytesFromString(h) ++ accum)
        //skip the empty string
        case h :: t if (h == "") => loop(t,accum)
        case h :: t if (h == "0") => loop(t, OP_0 :: accum)
        case h :: t if (ScriptOperationFactory.fromString(h).isDefined) =>
          loop(t,ScriptOperationFactory.fromString(h).get :: accum)
        case h :: t => loop(t, ScriptConstantImpl(h) :: accum)
        case Nil => accum
      }
    }


    //if the given string is hex, it is pretty straightforward to parse it
    if (ScalacoinUtil.isHex(str)) {
      //convert the hex string to a byte array and parse it
      val bytes = ScalacoinUtil.decodeHex(str)
      parse(bytes)
    } else {
      //this handles weird cases for parsing with various formats in bitcoin core.
      //take a look at https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/core_read.cpp#L53-L88
      //for the offical parsing algorithm, for examples of weird formats look inside of
      //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json
      loop(str.split(" ").toList, List()).reverse
    }

  }


  /**
   * Parses a byte array into a the asm operations for a script
   * will throw an exception if it fails to parse a op code
   * @param bytes
   * @return
   */
  def parse(bytes : List[Byte]) : List[ScriptToken] = {
    logger.info("Parsing a byte array into a list of script tokens")
    @tailrec
    def loop(bytes : List[Byte], accum : List[ScriptToken]) : List[ScriptToken] = {
      bytes match {
        case h :: t =>
          logger.debug("Op for matching: " + h.toByte)
          val op  = ScriptOperationFactory.fromByte(h).get
          //means that we need to push OP_0 onto the stack
          if (op == OP_0) loop(t,OP_0 :: accum)
          //means that we need to push x amount of bytes on to the stack
          else if (ScriptNumberFactory.operations.contains(op)) {
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

  /**
   * Parses the bytes in string format, an example input would look like this
   * "0x09 0x00000000 0x00000000 0x10"
   * see https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L21-L25
   * for examples of this
   * @param s
   * @return
   */
  private def parseBytesFromString(s: String) : List[ScriptConstant] = {
    val hexStrings : List[String] = (raw"\b0x([0-9a-f]+)\b".r
      .findAllMatchIn(s)
      .map(g => Integer.parseInt(g.group(1), 16).toHexString)
      .toList)
    val paddedHexStrings = hexStrings.map(hex => if (hex.size == 1) "0"+hex else hex )
    logger.debug("Padded hex strings: " + paddedHexStrings)
    //TODO: Figure out a better way to do this without calling .get on the result of fromByte
    val constants = paddedHexStrings.map(ScriptConstantImpl(_))
    constants
  }
}

object ScriptParser extends ScriptParser
