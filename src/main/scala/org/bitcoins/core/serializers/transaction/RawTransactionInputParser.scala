package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
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
    val numInputs = bytes.head.toInt
    @tailrec
    def loop(bytes : List[Byte], accum : List[TransactionInput], inputsLeftToParse : Int) : Seq[TransactionInput] = {
      if (inputsLeftToParse > 0) {
        val (txInput,bytesToBeParsed) = parseTransactionInput(bytes)
        val newAccum =  txInput :: accum
        val inputsLeft = inputsLeftToParse - 1
        loop(bytesToBeParsed.toList, newAccum,inputsLeft)
      } else accum
    }

    loop(bytes.tail, List(), numInputs).reverse
  }

  override def write(inputs : Seq[TransactionInput]) = {
    val serializedInputs : Seq[String] = for {
      input <- inputs
    } yield write(input)
    val inputsSize = CompactSizeUInt(inputs.length)
    inputsSize.hex + serializedInputs.mkString
  }


  /**
   * Writes a single transaction input
   * @param input
   * @return
   */
  def write(input : TransactionInput) : String = {
    val outPoint = RawTransactionOutPointParser.write(input.previousOutput)
    val varInt = input.scriptSigCompactSizeUInt.hex
    val scriptSig = RawScriptSignatureParser.write(input.scriptSignature)
    val sequence = addPadding(8,BitcoinSUtil.flipEndianess(input.sequence.hex))
    outPoint + varInt + scriptSig + sequence
  }


  /**
    * Parses a single [[TransactionInput]] from a sequence of bytes
    * @param bytes
    * @return
    */
  private def parseTransactionInput(bytes : Seq[Byte]): (TransactionInput,Seq[Byte]) = {
    logger.debug("Bytes to parse for input: " + BitcoinSUtil.encodeHex(bytes))
    val outPointBytesSize = 36
    val outPointBytes = bytes.take(outPointBytesSize)
    val outPoint = TransactionOutPoint(outPointBytes)

    val scriptSigCompactSizeUInt : CompactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(
      bytes.slice(outPointBytesSize,bytes.length))

    val scriptSigBytes = bytes.slice(outPointBytesSize + scriptSigCompactSizeUInt.size.toInt,
      outPointBytesSize + scriptSigCompactSizeUInt.size.toInt + scriptSigCompactSizeUInt.num.toInt)

    val scriptSig : ScriptSignature = RawScriptSignatureParser.read(scriptSigBytes)

    val sequenceBytesSize = 4
    val endOfScriptSigBytes = outPointBytesSize + scriptSigCompactSizeUInt.size.toInt + scriptSigBytes.length
    val lastInputByte = endOfScriptSigBytes + sequenceBytesSize
    val sequenceBytes = bytes.slice(endOfScriptSigBytes,lastInputByte)
    val sequenceNumberHex : String = BitcoinSUtil.encodeHex(sequenceBytes)
    val sequenceNumberFlippedEndianess = BitcoinSUtil.flipEndianess(sequenceNumberHex)
    val sequenceNumber : UInt32 = UInt32(sequenceNumberFlippedEndianess)
    val txInput = TransactionInput(outPoint,scriptSig,sequenceNumber)
    (txInput, bytes.slice(lastInputByte, bytes.length))
  }
}

object RawTransactionInputParser extends RawTransactionInputParser



