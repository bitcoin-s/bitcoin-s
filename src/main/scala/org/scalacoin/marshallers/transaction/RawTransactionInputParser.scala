package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.marshallers.script.RawScriptSignatureParser
import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.{TransactionInputImpl, TransactionOutPoint, TransactionInput}
import org.scalacoin.util.ScalacoinUtil
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
        logger.debug("Bytes to parse for input: " + ScalacoinUtil.encodeHex(bytes))
        val outPointBytesSize = 36
        val outPointBytes = bytes.take(outPointBytesSize)
        val outPoint : TransactionOutPoint  = RawTransactionOutPointParser.read(outPointBytes)
        val scriptSigSize = Integer.parseInt(ScalacoinUtil.encodeHex(bytes(outPointBytesSize)),16)
        val scriptSigBytes = bytes.slice(outPointBytesSize, outPointBytesSize + scriptSigSize + 1)
        val scriptSig : ScriptSignature = RawScriptSignatureParser.read(scriptSigBytes)

        val sequenceBytesSize = 4
        val lastInputByte = outPointBytesSize + scriptSigSize + sequenceBytesSize + 1
        val sequenceBytes = bytes.slice(outPointBytesSize + scriptSigSize + 1,lastInputByte)
        val sequenceNumberHex : String = ScalacoinUtil.encodeHex(sequenceBytes)
        val sequenceNumber : Long = BigInt(sequenceNumberHex,16).toLong
        val txInput = TransactionInputImpl(outPoint, scriptSig,sequenceNumber)

        val newAccum =  txInput :: accum
        val bytesToBeParsed = bytes.slice(lastInputByte, bytes.size)
        val inputsLeft = inputsLeftToParse - 1
        logger.debug("Parsed tx input: " + txInput)
        logger.debug("Inputs to be parsed " +  inputsLeft)
        loop(bytesToBeParsed, newAccum,inputsLeft)
      } else accum
    }

    loop(bytes.tail, List(), numInputs).reverse
  }

  override def write(inputs : Seq[TransactionInput]) = {
    val serializedInputs : Seq[String] = for {
      input <- inputs
    } yield {
        val outPoint = RawTransactionOutPointParser.write(input.previousOutput)
        val scriptSig = RawScriptSignatureParser.write(input.scriptSignature)
        val sequenceWithoutPadding = input.sequence.toHexString
        val paddingNeeded = 8 - sequenceWithoutPadding.size
        val padding = for { i <- 0 until paddingNeeded} yield "0"
        val sequence = sequenceWithoutPadding + padding.mkString
        outPoint + scriptSig + sequence
      }

    val inputsSizeWithoutPadding = inputs.size.toHexString
    val inputsSize = if (inputsSizeWithoutPadding.size == 1) "0" + inputsSizeWithoutPadding else inputsSizeWithoutPadding
    logger.debug("Input size: " + inputsSize)
    inputsSize + serializedInputs.mkString
  }
}

object RawTransactionInputParser extends RawTransactionInputParser



