package org.bitcoins.marshallers.transaction

import org.bitcoins.marshallers.RawBitcoinSerializer
import org.bitcoins.marshallers.script.RawScriptSignatureParser
import org.bitcoins.protocol.{CompactSizeUInt}
import org.bitcoins.protocol.script.ScriptSignature
import org.bitcoins.protocol.transaction.{TransactionInputImpl, TransactionOutPoint, TransactionInput}
import org.bitcoins.util.{BitcoinSUtil}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/13/16.
 * https://bitcoin.org/en/developer-reference#txin
 */
trait RawTransactionInputParser extends RawBitcoinSerializer[Seq[TransactionInput]] {

  private lazy val logger = LoggerFactory.getLogger(this.getClass().toString())

  override def read(bytes : List[Byte]) : Seq[TransactionInput] = {
    require(bytes.size > 0, "You passed in an empty list to read")
    val numInputs = bytes.head.toInt
    @tailrec
    def loop(bytes : List[Byte], accum : List[TransactionInput], inputsLeftToParse : Int) : Seq[TransactionInput] = {
      if (inputsLeftToParse > 0) {
        //TODO: This needs to be refactored into a loop function that returns a single TransactionInput
        //then call it multiple times and create a Seq[TransactionInput
        logger.debug("Bytes to parse for input: " + BitcoinSUtil.encodeHex(bytes))
        val outPointBytesSize = 36
        val outPointBytes = bytes.take(outPointBytesSize)
        val outPoint : TransactionOutPoint  = RawTransactionOutPointParser.read(outPointBytes)

        val scriptCompactSizeUIntSize : Int = BitcoinSUtil.parseCompactSizeUIntSize(bytes(outPointBytesSize)).toInt
        logger.debug("VarInt hex: " + BitcoinSUtil.encodeHex(bytes.slice(outPointBytesSize,outPointBytesSize + scriptCompactSizeUIntSize)))
        val scriptSigCompactSizeUInt : CompactSizeUInt = BitcoinSUtil.parseCompactSizeUInt(bytes.slice(outPointBytesSize,outPointBytesSize + scriptCompactSizeUIntSize))


        val scriptSigBytes = bytes.slice(outPointBytesSize+ scriptCompactSizeUIntSize,
          outPointBytesSize +  scriptCompactSizeUIntSize + scriptSigCompactSizeUInt.num.toInt)

        val scriptSig : ScriptSignature = RawScriptSignatureParser.read(scriptSigBytes)

        val sequenceBytesSize = 4
        val endOfScriptSigBytes = outPointBytesSize + scriptSigCompactSizeUInt.num.toInt + scriptCompactSizeUIntSize
        val lastInputByte = endOfScriptSigBytes + sequenceBytesSize
        val sequenceBytes = bytes.slice(endOfScriptSigBytes,lastInputByte)
        logger.debug("Sequence bytes: " + BitcoinSUtil.encodeHex(sequenceBytes))
        val sequenceNumberHex : String = BitcoinSUtil.encodeHex(sequenceBytes)
        val sequenceNumber : Long = java.lang.Long.parseLong(sequenceNumberHex,16)
        val txInput = TransactionInputImpl(outPoint,scriptSig,sequenceNumber)

        val newAccum =  txInput :: accum
        val bytesToBeParsed = bytes.slice(lastInputByte, bytes.size)
        val inputsLeft = inputsLeftToParse - 1

        loop(bytesToBeParsed, newAccum,inputsLeft)
      } else accum
    }

    loop(bytes.tail, List(), numInputs).reverse
  }

  override def write(inputs : Seq[TransactionInput]) = {
    val serializedInputs : Seq[String] = for {
      input <- inputs
    } yield write(input)

    val inputsSizeWithoutPadding = inputs.size.toHexString
    val inputsSize = if (inputsSizeWithoutPadding.size == 1) "0" + inputsSizeWithoutPadding else inputsSizeWithoutPadding
    logger.debug("Input size: " + inputsSize)
    inputsSize + serializedInputs.mkString
  }


  /**
   * Writes a single transaction input
 *
   * @param input
   * @return
   */
  def write(input : TransactionInput) : String = {
    val outPoint = RawTransactionOutPointParser.write(input.previousOutput)
    val varInt = input.scriptSigCompactSizeUInt.hex
    val scriptSig = RawScriptSignatureParser.write(input.scriptSignature)
    val sequenceWithoutPadding = input.sequence.toHexString
    val paddingNeeded = 8 - sequenceWithoutPadding.size
    val padding = for { i <- 0 until paddingNeeded} yield "0"

    val sequence = sequenceWithoutPadding + padding.mkString
    outPoint + varInt + scriptSig + sequence
  }
}

object RawTransactionInputParser extends RawTransactionInputParser



