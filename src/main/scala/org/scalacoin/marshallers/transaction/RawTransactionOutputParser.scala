package org.scalacoin.marshallers.transaction

import org.scalacoin.currency.{CurrencyUnits, Satoshis}
import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.protocol.transaction.{TransactionOutputImpl, TransactionOutput}
import org.scalacoin.util.ScalacoinUtil

import scala.annotation.tailrec

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
trait RawTransactionOutputParser extends RawBitcoinSerializer[Seq[TransactionOutput]] with ScriptParser {


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
        val newAccum = TransactionOutputImpl(satoshis, 0, script) :: accum
        val bytesToBeParsed = bytes.slice(firstScriptPubKeyByte + scriptPubKeySize, bytes.size)
        loop(bytesToBeParsed, newAccum, outputsLeftToParse-1)
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
      val satoshisHexWithoutPadding : String =
        ScalacoinUtil.encodeHex(ScalacoinUtil.decodeHex(ScalacoinUtil.encodeHex(satoshis)).reverse).reverse
      val paddingNeeded = 16 - satoshisHexWithoutPadding.size
      val padding : Seq[String] = for ( i <- 0 until paddingNeeded) yield "0"
      val satoshisHex = padding.mkString + satoshisHexWithoutPadding
      satoshisHex.reverse + output.scriptPubKey.hex
    }
    numOutputs + serializedOutputs.mkString
  }
}


object RawTransactionOutputParser extends RawTransactionOutputParser