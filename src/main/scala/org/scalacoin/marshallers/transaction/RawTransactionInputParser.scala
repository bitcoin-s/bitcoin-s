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
        //require(outPoint.txId == "e99eb3e6551844d0db252ef242c043796b3b0ccfb126c0ae09f9dd0230e2f10d")
        val scriptVarIntSize : Int = ScalacoinUtil.parseVarIntSize(bytes(outPointBytesSize)).toInt
        //require(scriptVarIntSize == 3, scriptVarIntSize +" was not equal to 2")
        logger.debug("VarInt hex: " + ScalacoinUtil.encodeHex(bytes.slice(outPointBytesSize,outPointBytesSize + scriptVarIntSize)))
        val scriptSigVarInt : VarInt = ScalacoinUtil.parseVarInt(bytes.slice(outPointBytesSize,outPointBytesSize + scriptVarIntSize))
        //require(scriptSigVarInt == VarIntImpl(253,3), scriptSigVarInt + " was not what we expected")
        logger.debug("Outpoint hex: " + outPoint.hex)

        logger.debug("Script VarInt bytes: " + bytes(outPointBytesSize))
        logger.debug("Script VarInt Size: " + scriptSigVarInt)
        val scriptSigBytes = bytes.slice(outPointBytesSize+ scriptVarIntSize,
          outPointBytesSize +  scriptVarIntSize + scriptSigVarInt.num.toInt)

        val scriptSig : ScriptSignature = RawScriptSignatureParser.read(scriptSigBytes)
        //require(scriptSig.hex == "004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae",
          //scriptSig.hex + " was not the expected hex value")
        logger.debug("Script sig hex: " + scriptSig.hex)

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
        val varInt = input.scriptSigVarInt.hex
        val scriptSig = RawScriptSignatureParser.write(input.scriptSignature)
        val sequenceWithoutPadding = input.sequence.toHexString
        val paddingNeeded = 8 - sequenceWithoutPadding.size
        val padding = for { i <- 0 until paddingNeeded} yield "0"

        val sequence = sequenceWithoutPadding + padding.mkString
        outPoint + varInt + scriptSig + sequence
      }

    val inputsSizeWithoutPadding = inputs.size.toHexString
    val inputsSize = if (inputsSizeWithoutPadding.size == 1) "0" + inputsSizeWithoutPadding else inputsSizeWithoutPadding
    logger.debug("Input size: " + inputsSize)
    inputsSize + serializedInputs.mkString
  }
}

object RawTransactionInputParser extends RawTransactionInputParser



