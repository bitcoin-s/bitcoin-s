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
   * example: ["0", "IF 0x50 ENDIF 1", "P2SH,STRICTENC", "0x50 is reserved (ok if not executed)"] (from script_valid.json)
   * @param str
   * @return
   */
  def parse(str : String) : List[ScriptToken] = {
    logger.info("Parsing string: " + str + " into a list of script tokens")

    @tailrec
    def loop(operations : List[String], accum : List[ScriptToken]) : List[ScriptToken] = {
      logger.debug("Attempting to parse: " + operations.headOption)
      operations match {
        //for parsing strings like 'Az', need to remove single quotes
        //example: https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L24
        case h :: t if (h.size > 0 && h.head == ''' && h.last == ''') =>
          val strippedQuotes = h.replace("'","")
          val hex = ScalacoinUtil.encodeHex(strippedQuotes.getBytes)
          loop(t, ScriptConstantImpl(hex) :: accum)
        //if we see a byte constant of just 0x09
        //parse the characters as a hex op
        case h :: t if (h.size == 4 && h.substring(0,2) == "0x") =>
          val hexString = h.substring(2,h.size)
          logger.debug("Hex string: " + hexString)
          loop(t,ScriptOperationFactory.fromHex(hexString).get :: accum)

        //if we see a byte constant in the form of "0x09adb"
        case h  :: t if (h.size > 1 && h.substring(0,2) == "0x") => loop(t,parseBytesFromString(h) ++ accum)
        //skip the empty string
        case h :: t if (h == "") => loop(t,accum)
        case h :: t if (h == "0") => loop(t, OP_0 :: accum)
        case h :: t if (ScriptOperationFactory.fromString(h).isDefined) =>
          val op = ScriptOperationFactory.fromString(h).get
          val parsingHelper : ParsingHelper[String] = parseOperationString(op,accum,t)
          loop(parsingHelper.tail,parsingHelper.accum)

        case h :: t => loop(t, ScriptConstantImpl(h) :: accum)
        case Nil => accum
      }
    }


    //if the given string is hex, it is pretty straight forward to parse it
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
    logger.info("Parsing byte list: " + bytes + " into a list of script tokens")
    @tailrec
    def loop(bytes : List[Byte], accum : List[ScriptToken]) : List[ScriptToken] = {
      logger.debug("Byte to be parsed: " + bytes.headOption)
      bytes match {
        case h :: t =>
          val op  = ScriptOperationFactory.fromByte(h).get
          val parsingHelper : ParsingHelper[Byte] = parseOperationByte(op,accum,t)
          loop(parsingHelper.tail,parsingHelper.accum)
        case Nil => accum
      }
    }
    loop(bytes, List()).reverse
  }

  /**
   * Slices the amount of bytes specified in the op parameter and then creates a script constant
   * from those bytes. Returns the script constant and the byte array without the script constant
   * @param op
   * @param bytes
   * @return
   */
  private def sliceConstant[T](op : ScriptNumber, data : List[T]) : (List[T], List[T]) = {
    val finalIndex = op.opCode
    val dataConstant = data.slice(0,finalIndex)
    (dataConstant,data.slice(finalIndex,data.size))
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
    logger.debug("Parsing bytes from string " + s)
    val scriptConstants : List[ScriptConstant] = (raw"\b0x([0-9a-f]+)\b".r
      .findAllMatchIn(s)
      .map(g => ScriptConstantImpl(g.group(1)))
      .toList)
    scriptConstants
  }


  case class ParsingHelper[T](tail : List[T], accum : List[ScriptToken])
  /**
   * Parses an operation if the tail is a List[Byte]
   * If the operation is a script number, it pushes the number of bytes onto the stack
   * specified by the script number
   * i.e. If the operation was ScriptNumber(5), it would slice 5 bytes off of the tail and
   * places them into a ScriptConstant and add them to the accumulator.
   * @param op
   * @param accum
   * @param tail
   * @return
   */
  private def parseOperationByte(op : ScriptOperation, accum : List[ScriptToken], tail : List[Byte]) : ParsingHelper[Byte] = {
    op match {
      case scriptNumber : ScriptNumberImpl =>
        //means that we need to push x amount of bytes on to the stack
        val (constant,newTail) = sliceConstant(scriptNumber,tail)
        val scriptConstant = new ScriptConstantImpl(constant)
        ParsingHelper(newTail,scriptConstant :: accum)

      case _ =>
        //means that we need to push the operation onto the stack
        ParsingHelper(tail,op :: accum)
    }
  }

  /**
   * Parses an operation if the tail is a List[String]
   * If the operation is a script number, it pushes the number of bytes onto the stack
   * specified by the script number
   * i.e. If the operation was ScriptNumber(5), it would slice 5 bytes off of the tail and
   * places them into a ScriptConstant and add them to the accumulator.
   * @param op
   * @param accum
   * @param tail
   * @return
   */
  private def parseOperationString(op : ScriptOperation, accum : List[ScriptToken], tail : List[String]) : ParsingHelper[String] = {
    op match {
      case scriptNumber : ScriptNumberImpl =>
        //means that we need to push x amount of bytes on to the stack
        val (constant,newTail) = sliceConstant[String](scriptNumber,tail)
        val scriptConstant = ScriptConstantImpl(constant.mkString)
        ParsingHelper(newTail,scriptConstant :: accum)

      case _ =>
        //means that we need to push the operation onto the stack
        ParsingHelper(tail,op :: accum)
    }
  }
}

object ScriptParser extends ScriptParser
