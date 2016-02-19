package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.marshallers.script.RawScriptSignatureParser
import org.scalacoin.protocol.{VarIntImpl, VarInt}
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

        val scriptVarIntSize : Int = ScalacoinUtil.parseVarIntSize(bytes(outPointBytesSize)).toInt
        logger.debug("VarInt hex: " + ScalacoinUtil.encodeHex(bytes.slice(outPointBytesSize,outPointBytesSize + scriptVarIntSize)))
        val scriptSigVarInt : VarInt = ScalacoinUtil.parseVarInt(bytes.slice(outPointBytesSize,outPointBytesSize + scriptVarIntSize))

        val scriptSigBytes = bytes.slice(outPointBytesSize + scriptVarIntSize,
          outPointBytesSize +  scriptVarIntSize + scriptSigVarInt.num.toInt)

        val scriptSig : ScriptSignature = RawScriptSignatureParser.read(scriptSigBytes)

        val sequenceBytesSize = 4
        val endOfScriptSigBytes = outPointBytesSize + scriptSigVarInt.num.toInt + scriptVarIntSize
        val lastInputByte = endOfScriptSigBytes + sequenceBytesSize
        val sequenceBytes = bytes.slice(endOfScriptSigBytes,lastInputByte)
        logger.debug("Sequence bytes: " + ScalacoinUtil.encodeHex(sequenceBytes))
        val sequenceNumberHex : String = ScalacoinUtil.encodeHex(sequenceBytes)
        val sequenceNumber : Long = java.lang.Long.parseLong(sequenceNumberHex,16)
        val txInput = TransactionInputImpl(outPoint,scriptSigVarInt, scriptSig,sequenceNumber)

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
   * @param input
   * @return
   */
  def write(input : TransactionInput) : String = {
    val outPoint = RawTransactionOutPointParser.write(input.previousOutput)
    val varInt = input.scriptSigVarInt.hex
    val scriptSig = RawScriptSignatureParser.write(input.scriptSignature)
    val sequenceWithoutPadding = input.sequence.toHexString
    val paddingNeeded = 8 - sequenceWithoutPadding.size
    val padding = for { i <- 0 until paddingNeeded} yield "0"

    val sequence = sequenceWithoutPadding + padding.mkString
    outPoint + varInt + scriptSig + sequence
  }
}

object RawTransactionInputParser extends RawTransactionInputParser



