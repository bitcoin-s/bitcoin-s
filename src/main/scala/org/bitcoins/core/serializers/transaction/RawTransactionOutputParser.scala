package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.serializers.script.{RawScriptPubKeyParser, ScriptParser}
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSatoshisSerializer}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}

import scala.annotation.tailrec

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
trait RawTransactionOutputParser extends RawBitcoinSerializer[Seq[TransactionOutput]] with ScriptParser with BitcoinSLogger {

  /** Reads a sequence of outputs, expects first element in the byte array to be a [[org.bitcoins.core.protocol.CompactSizeUInt]]
    * indicating how many outputs we need to read
    */
  override def read(bytes : List[Byte]) : Seq[TransactionOutput] = {

    val numOutputs = CompactSizeUInt.parseCompactSizeUInt(bytes)
    @tailrec
    def loop(bytes : List[Byte], accum : List[TransactionOutput], outputsLeftToParse : Int) : List[TransactionOutput] = {
      if (outputsLeftToParse > 0) {
        val parsedOutput = readSingleOutput(bytes)
        val newAccum =  parsedOutput :: accum
        val bytesToBeParsed = bytes.slice(parsedOutput.size, bytes.size)
        val outputsLeft = outputsLeftToParse-1
        loop(bytesToBeParsed, newAccum, outputsLeft)
      } else accum
    }
    val (_,outputBytes) = bytes.splitAt(numOutputs.size.toInt)
    loop(outputBytes,Nil,numOutputs.num.toInt).reverse
  }

  override def write(outputs : Seq[TransactionOutput]) : String = {
    val numOutputs = CompactSizeUInt(UInt64(outputs.length))
    val serializedOutputs : Seq[String] = for {
      output <- outputs
    } yield write(output)
    numOutputs.hex + serializedOutputs.mkString
  }

  /** Writes a single transaction output */
  def write(output : TransactionOutput) : String = {
    val satoshis = CurrencyUnits.toSatoshis(output.value)
    val satoshisHexWithoutPadding : String = BitcoinSUtil.flipEndianness(satoshis.hex)
    val satoshisHex = addPadding(16,satoshisHexWithoutPadding)
    logger.debug("satoshis: " + satoshisHex)
/*    if (compactSizeUIntHex == "00") satoshisHex + compactSizeUIntHex
    else */ satoshisHex + output.scriptPubKey.hex
  }

  /** Reads a single output from the given bytes, note this is different than [[org.bitcoins.core.serializers.transaction.RawTransactionOutputParser.read]]
    * because it does NOT expect a [[CompactSizeUInt]] to be the first element in the byte array indicating how many outputs we have
    */
  def readSingleOutput(bytes: Seq[Byte]): TransactionOutput = {
    val satoshisHex = BitcoinSUtil.encodeHex(bytes.take(8).reverse)
    logger.debug("Satoshi hex: " + satoshisHex)
    val satoshis = parseSatoshis(satoshisHex)
    //it doesn't include itself towards the size, thats why it is incremented by one
    val scriptPubKeyBytes = bytes.slice(8, bytes.size)
    val scriptPubKey = RawScriptPubKeyParser.read(scriptPubKeyBytes)
    val parsedOutput = TransactionOutput(satoshis,scriptPubKey)
    logger.debug("Parsed output: " + parsedOutput)
    parsedOutput
  }

  /**
    * Parses the amount of [[Satoshis]] a hex string represents. */
  private def parseSatoshis(hex : String) : Satoshis = RawSatoshisSerializer.read(hex)
}


object RawTransactionOutputParser extends RawTransactionOutputParser