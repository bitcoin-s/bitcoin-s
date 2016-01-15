package org.scalacoin.marshallers.transaction

import org.scalacoin.currency.{CurrencyUnits, Satoshis}
import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.protocol.transaction.{TransactionOutputImpl, TransactionOutput}
import org.scalacoin.util.ScalacoinUtil
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
trait RawTransactionOutputParser extends RawBitcoinSerializer[Seq[TransactionOutput]] with ScriptParser {

  private lazy val logger = LoggerFactory.getLogger(this.getClass().toString())
  override def read(bytes : List[Byte]) : Seq[TransactionOutput] = {
    val numOutputs = bytes.head.toInt
    @tailrec
    def loop(bytes : List[Byte], accum : List[TransactionOutput], outputsLeftToParse : Int) : List[TransactionOutput] = {
      if (outputsLeftToParse > 0) {
        val satoshisHex = ScalacoinUtil.encodeHex(bytes.take(8).reverse)
        val satoshis = Satoshis(Integer.parseInt(satoshisHex, 16))
        //it doesn't include itself towards the size, thats why it is incremented by one
        val firstScriptPubKeyByte = 8
        val scriptPubKeySize = bytes(firstScriptPubKeyByte).toInt + 1
        val scriptPubKeyBytes = bytes.slice(firstScriptPubKeyByte, firstScriptPubKeyByte + scriptPubKeySize)
        val script = RawScriptPubKeyParser.read(scriptPubKeyBytes)
        val parsedOutput = TransactionOutputImpl(satoshis, 0, script)
        val newAccum =  parsedOutput:: accum
        val bytesToBeParsed = bytes.slice(firstScriptPubKeyByte + scriptPubKeySize, bytes.size)
        val outputsLeft = outputsLeftToParse-1
        logger.debug("Parsed output: " + parsedOutput)
        logger.debug("Outputs left to parse: " + outputsLeft)
        loop(bytesToBeParsed, newAccum, outputsLeft)
      } else accum
    }
    loop(bytes.tail,List(),numOutputs).reverse
  }

  override def write(outputs : Seq[TransactionOutput]) : String = {
    val numOutputs = ScalacoinUtil.encodeHex(outputs.size.toByte)
    val serializedOutputs : Seq[String] = for {
      output <- outputs
    } yield {
      val satoshis = CurrencyUnits.toSatoshis(output.value)
      //TODO: Clean this up, this is very confusing. If you remove this .reverse method calls you can see the unit test failing
      val satoshisHexWithoutPadding : String = ScalacoinUtil.encodeHex(satoshis)
        //ScalacoinUtil.encodeHex(ScalacoinUtil.decodeHex(ScalacoinUtil.encodeHex(satoshis)).reverse)
      val satoshisHex = addPadding(16,satoshisHexWithoutPadding)
      satoshisHex + output.scriptPubKey.hex
    }
    numOutputs + serializedOutputs.mkString
  }
}


object RawTransactionOutputParser extends RawTransactionOutputParser