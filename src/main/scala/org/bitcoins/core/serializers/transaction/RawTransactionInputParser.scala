package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{TransactionInput, TransactionOutPoint}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.serializers.script.RawScriptSignatureParser
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}

import scala.annotation.tailrec

/**
 * Created by chris on 1/13/16.
 * https://bitcoin.org/en/developer-reference#txin
 */
trait RawTransactionInputParser extends RawBitcoinSerializer[Seq[TransactionInput]] with BitcoinSLogger {


  override def read(bytes : List[Byte]) : Seq[TransactionInput] = {
    require(bytes.nonEmpty, "You passed in an empty list to read")
    val numInputs = CompactSizeUInt.parseCompactSizeUInt(bytes)
    @tailrec
    def loop(bytes : List[Byte], accum : List[TransactionInput], inputsLeftToParse : Int) : Seq[TransactionInput] = {
      if (inputsLeftToParse > 0) {
        val (txInput,bytesToBeParsed) = parseTransactionInput(bytes)
        val newAccum =  txInput :: accum
        val inputsLeft = inputsLeftToParse - 1
        loop(bytesToBeParsed.toList, newAccum,inputsLeft)
      } else accum
    }
    logger.debug("Num inputs: " + numInputs.num)
    val inputBytes = bytes.slice(numInputs.size.toInt, bytes.size)
    loop(inputBytes, Nil, numInputs.num.toInt).reverse
  }

  override def write(inputs : Seq[TransactionInput]) = {
    val serializedInputs : Seq[String] = for {
      input <- inputs
    } yield write(input)
    val inputsSize = CompactSizeUInt(UInt64(inputs.length))
    inputsSize.hex + serializedInputs.mkString
  }


  /** Writes a single transaction input */
  def write(input : TransactionInput) : String = {
    val outPoint = RawTransactionOutPointParser.write(input.previousOutput)
    val scriptSig = RawScriptSignatureParser.write(input.scriptSignature)
    val sequence = addPadding(8,BitcoinSUtil.flipEndianness(input.sequence.hex))
    outPoint + scriptSig + sequence
  }


  /** Parses a single [[TransactionInput]] from a sequence of bytes */
  private def parseTransactionInput(bytes : Seq[Byte]): (TransactionInput,Seq[Byte]) = {
    logger.debug("Bytes to parse for input: " + BitcoinSUtil.encodeHex(bytes))
    val outPointBytesSize = 36
    val outPointBytes = bytes.take(outPointBytesSize)
    val outPoint = TransactionOutPoint(outPointBytes)

    val scriptSigBytes = bytes.slice(outPointBytesSize,bytes.size)
    logger.debug("Scriptsig bytes: " + BitcoinSUtil.encodeHex(scriptSigBytes))
    val scriptSig : ScriptSignature = RawScriptSignatureParser.read(scriptSigBytes)
    logger.debug("Script sig parsed bytes: " + BitcoinSUtil.encodeHex(scriptSig.bytes))
    val sequenceBytesSize = 4
    val endOfScriptSigBytes = outPointBytesSize + scriptSig.bytes.size
    val lastInputByte = endOfScriptSigBytes + sequenceBytesSize
    val sequenceBytes = bytes.slice(endOfScriptSigBytes,lastInputByte)
    val sequenceNumberHex : String = BitcoinSUtil.encodeHex(sequenceBytes)
    val sequenceNumberFlippedEndianness = BitcoinSUtil.flipEndianness(sequenceNumberHex)
    logger.debug("Sequence number hex: " + sequenceNumberFlippedEndianness)
    val sequenceNumber : UInt32 = UInt32(sequenceNumberFlippedEndianness)
    val txInput = TransactionInput(outPoint,scriptSig,sequenceNumber)
    logger.debug("Parsed input: " + txInput)
    (txInput, bytes.slice(lastInputByte, bytes.length))
  }
}

object RawTransactionInputParser extends RawTransactionInputParser



