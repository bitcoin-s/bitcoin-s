package org.bitcoins.core.serializers.script

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.script._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.util.{ BitcoinSLogger, BitcoinSUtil, Factory }

import scala.annotation.tailrec
import scala.util.Try

/**
 * Created by chris on 1/7/16.
 */
sealed abstract class ScriptParser extends Factory[List[ScriptToken]] {

  /** Parses a list of bytes into a list of script tokens */
  def fromBytes(bytes: Seq[Byte]): List[ScriptToken] = {
    val scriptTokens: List[ScriptToken] = parse(bytes)
    scriptTokens
  }

  /**
   * Parses an asm output script of a transaction
   * example: "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"
   * example: ["0", "IF 0x50 ENDIF 1", "P2SH,STRICTENC", "0x50 is reserved (ok if not executed)"] (from script_valid.json)
   */
  def fromString(str: String): List[ScriptToken] = {
    if (str.size > 1 && str.substring(0, 2) == "0x" && str.split(" ").size == 1) {
      //parse this as a byte array that is led with a 0x for example
      //0x4e03000000ffff
      val hex = str.substring(2, str.size)
      fromBytes(BitcoinSUtil.decodeHex(hex))
    } else {
      val scriptTokens: List[ScriptToken] = parse(str)
      scriptTokens
    }
  }

  /**
   * Parses a string to a sequence of script tokens
   * example: "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"
   * example: ["0", "IF 0x50 ENDIF 1", "P2SH,STRICTENC", "0x50 is reserved (ok if not executed)"] (from script_valid.json)
   */
  private def parse(str: String): List[ScriptToken] = {
    @tailrec
    def loop(operations: List[String], accum: List[Byte]): List[Byte] = {
      /*      logger.debug("Attempting to parse: " + operations.headOption)
      logger.debug("Accum: " + accum)*/
      operations match {
        //for parsing strings like 'Az', need to remove single quotes
        //example: [[https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L24]]
        case h :: t if (h.size > 0 && h.head == ''' && h.last == ''') =>
          val strippedQuotes = h.replace("'", "")
          if (strippedQuotes.size == 0) {
            loop(t, OP_0.bytes.toList ++ accum)
          } else {
            val bytes: Seq[Byte] = BitcoinSUtil.decodeHex(BitcoinSUtil.flipEndianness(strippedQuotes.getBytes.toList))

            val bytesToPushOntoStack: List[ScriptToken] = (bytes.size > 75) match {
              case true =>
                val scriptNumber = ScriptNumber(BitcoinSUtil.flipEndianness(ScriptNumberUtil.longToHex(bytes.size)))
                bytes.size match {
                  case size if size < Byte.MaxValue =>
                    List(scriptNumber, OP_PUSHDATA1)
                  case size if size < Short.MaxValue =>
                    List(scriptNumber, OP_PUSHDATA2)
                  case size if size < Int.MaxValue =>
                    List(scriptNumber, OP_PUSHDATA4)
                }
              case false => List(BytesToPushOntoStack(bytes.size))
            }

            loop(t, bytes.toList ++ bytesToPushOntoStack.flatMap(_.bytes) ++ accum)
          }
        //if we see a byte constant in the form of "0x09adb"
        case h :: t if (h.size > 1 && h.substring(0, 2) == "0x") =>
          loop(t, BitcoinSUtil.decodeHex(h.substring(2, h.size).toLowerCase).toList.reverse ++ accum)
        //skip the empty string
        case h :: t if (h == "")  => loop(t, accum)
        case h :: t if (h == "0") => loop(t, OP_0.bytes.toList ++ accum)

        case h :: t if (ScriptOperation.fromString(h).isDefined) =>
          val op = ScriptOperation.fromString(h).get
          loop(t, op.bytes.toList ++ accum)
        case h :: t if (tryParsingLong(h)) =>
          val hexLong = BitcoinSUtil.flipEndianness(ScriptNumberUtil.longToHex(h.toLong))
          val bytesToPushOntoStack = BytesToPushOntoStack(hexLong.size / 2)
          //convert the string to int, then convert to hex
          loop(t, BitcoinSUtil.decodeHex(hexLong).toList ++ bytesToPushOntoStack.bytes.toList ++ accum)
        //means that it must be a BytesToPushOntoStack followed by a script constant
        case h :: t =>
          //find the size of the string in bytes
          val bytesToPushOntoStack = BytesToPushOntoStack(h.size / 2)
          loop(t, BitcoinSUtil.decodeHex(BitcoinSUtil.flipEndianness(h)).toList ++ bytesToPushOntoStack.bytes.toList ++ accum)
        case Nil => accum
      }
    }
    if (tryParsingLong(str) && str.size > 1 && str.substring(0, 2) != "0x") {
      //for the case when there is just a single decimal constant
      //i.e. "8388607"
      val scriptNumber = ScriptNumber(parseLong(str))
      val bytesToPushOntoStack = BytesToPushOntoStack(scriptNumber.bytes.size)
      List(bytesToPushOntoStack, scriptNumber)
    } else if (BitcoinSUtil.isHex(str) && str.toLowerCase == str) {
      //if the given string is hex, it is pretty straight forward to parse it
      //convert the hex string to a byte array and parse it
      val bytes = BitcoinSUtil.decodeHex(str)
      parse(bytes)
    } else {
      //this handles weird cases for parsing with various formats in bitcoin core.
      //take a look at https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/core_read.cpp#L53-L88
      //for the offical parsing algorithm, for examples of weird formats look inside of
      //[[https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json]]
      val parsedBytesFromString = loop(str.split(" ").toList, List()).reverse
      parse(parsedBytesFromString)
    }
  }

  /**
   * Parses a byte array into a the asm operations for a script
   * will throw an exception if it fails to parse a op code
   */
  private def parse(bytes: List[Byte]): List[ScriptToken] = {
    @tailrec
    def loop(bytes: List[Byte], accum: List[ScriptToken]): List[ScriptToken] = {
      //logger.debug("Byte to be parsed: " + bytes.headOption)
      bytes match {
        case h :: t =>
          val op = ScriptOperation(h).get
          val parsingHelper: ParsingHelper[Byte] = parseOperationByte(op, accum, t)
          loop(parsingHelper.tail, parsingHelper.accum)
        case Nil => accum
      }
    }
    loop(bytes, List()).reverse

  }

  private def parse(bytes: Seq[Byte]): List[ScriptToken] = parse(bytes.toList)

  /** Parses a redeem script from the given script token */
  def parseRedeemScript(scriptToken: ScriptToken): Try[List[ScriptToken]] = {
    val redeemScript: Try[List[ScriptToken]] = Try(parse(scriptToken.bytes))
    redeemScript
  }

  /**
   * Slices the amount of bytes specified in the bytesToPushOntoStack parameter and then creates a script constant
   * from those bytes. Returns the script constant and the byte array without the script constant
   */
  private def sliceConstant[T](bytesToPushOntoStack: BytesToPushOntoStack, data: List[T]): (List[T], List[T]) = {
    val finalIndex = bytesToPushOntoStack.opCode
    val dataConstant = data.slice(0, finalIndex)
    (dataConstant, data.slice(finalIndex, data.size))
  }

  /**
   * Parses the bytes in string format, an example input would look like this
   * "0x09 0x00000000 0x00000000 0x10"
   * see [[https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L21-L25]]
   * for examples of this
   */
  def parseBytesFromString(s: String): List[ScriptConstant] = {
    //logger.debug("Parsing bytes from string " + s)
    val scriptConstants: List[ScriptConstant] = (raw"\b0x([0-9a-f]+)\b".r
      .findAllMatchIn(s.toLowerCase)
      .map(g =>
        // 1 hex = 4 bits therefore 16 hex characters * 4 bits = 64
        // if it is not smaller than 16 hex characters it cannot
        //fit inside of a scala long
        //therefore store it as a script constant
        if (g.group(1).size <= 16) {
          ScriptNumber(g.group(1))
        } else {
          ScriptConstant(g.group(1))
        }).toList)
    scriptConstants
  }

  private sealed case class ParsingHelper[T](tail: List[T], accum: List[ScriptToken])

  /**
   * Parses an operation if the tail is a List[Byte]
   * If the operation is a bytesToPushOntoStack, it pushes the number of bytes onto the stack
   * specified by the bytesToPushOntoStack
   * i.e. If the operation was BytesToPushOntoStackImpl(5), it would slice 5 bytes off of the tail and
   * places them into a ScriptConstant and add them to the accumulator.
   */
  private def parseOperationByte(op: ScriptOperation, accum: List[ScriptToken], tail: List[Byte]): ParsingHelper[Byte] = {
    op match {
      case bytesToPushOntoStack: BytesToPushOntoStack =>
        //logger.debug("Parsing operation byte: " +bytesToPushOntoStack )
        //means that we need to push x amount of bytes on to the stack
        val (constant, newTail) = sliceConstant(bytesToPushOntoStack, tail)
        val scriptConstant = ScriptConstant(constant)
        ParsingHelper(newTail, scriptConstant :: bytesToPushOntoStack :: accum)
      case OP_PUSHDATA1 => parseOpPushData(op, accum, tail)
      case OP_PUSHDATA2 => parseOpPushData(op, accum, tail)
      case OP_PUSHDATA4 => parseOpPushData(op, accum, tail)
      case _ =>
        //means that we need to push the operation onto the stack
        ParsingHelper(tail, op :: accum)
    }
  }

  /**
   * Parses OP_PUSHDATA operations correctly. Slices the appropriate amount of bytes off of the tail and pushes
   * them onto the accumulator.
   * @param op the script operation that is being parsed, this should be OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4 or else it throws an exception
   * @param accum the parsed script tokens so far
   * @param tail the bytes to be parsed still
   * @return
   */
  private def parseOpPushData(op: ScriptOperation, accum: List[ScriptToken], tail: List[Byte]): ParsingHelper[Byte] = {

    def parseOpPushDataHelper(numBytes: Int): ParsingHelper[Byte] = {
      //next numBytes is the size of the script constant
      val scriptConstantHex = tail.slice(0, numBytes)
      val uInt32Push = UInt32(BitcoinSUtil.flipEndianness(scriptConstantHex))
      //need this for the case where we have an OP_PUSHDATA4 with a number larger than a int32 can hold
      //TODO: Review this more, see this transaction's scriptSig as an example: b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
      val bytesForPushOp = Try(uInt32Push.toInt).getOrElse(Int.MaxValue)
      val bytesToPushOntoStack = ScriptConstant(scriptConstantHex)
      val scriptConstantBytes = tail.slice(numBytes, bytesForPushOp + numBytes)
      val scriptConstant = ScriptConstant(scriptConstantBytes)
      val restOfBytes = tail.slice(bytesForPushOp + numBytes, tail.size)
      buildParsingHelper(op, bytesToPushOntoStack, scriptConstant, restOfBytes, accum)
    }

    op match {
      case OP_PUSHDATA1 =>
        parseOpPushDataHelper(1)
      case OP_PUSHDATA2 =>
        parseOpPushDataHelper(2)
      case OP_PUSHDATA4 =>
        parseOpPushDataHelper(4)
      case _: ScriptToken => throw new RuntimeException("parseOpPushData can only parse OP_PUSHDATA operations")
    }
  }

  /**
   * Helper function to build the parsing helper for parsing an OP_PUSHDATA operation
   * @param op the OP_PUSHDATA operation being added to the accum
   * @param bytesToPushOntoStack the number of bytes that are pushed onto the stack by the OP_PUSHDATA operation
   * @param scriptConstant the constant that is being pushed onto the stack by the OP_PUSHDATA operation
   * @param restOfBytes the remaining bytes that need to be parsed
   * @param accum the accumulator filled with script tokens that have already been parsed
   * @return
   */
  private def buildParsingHelper(op: ScriptOperation, bytesToPushOntoStack: ScriptConstant,
                                 scriptConstant: ScriptConstant, restOfBytes: List[Byte], accum: List[ScriptToken]): ParsingHelper[Byte] = {
    if (bytesToPushOntoStack.hex == "00") {
      //if we need to push 0 bytes onto the stack we do not add the script constant
      ParsingHelper[Byte](
        restOfBytes,
        bytesToPushOntoStack :: op :: accum
      )
    } else ParsingHelper[Byte](
      restOfBytes,
      scriptConstant :: bytesToPushOntoStack :: op :: accum
    )
  }

  /** Checks if a string can be cast to an int */
  private def tryParsingLong(str: String): Boolean = Try(parseLong(str)).isSuccess

  private def parseLong(str: String) = {
    if (str.substring(0, 2) == "0x") {
      val strRemoveHex = str.substring(2, str.size)
      ScriptNumberUtil.toLong(strRemoveHex)
    } else str.toLong
  }
}

object ScriptParser extends ScriptParser
