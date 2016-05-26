package org.bitcoins.core.util

import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}

/**
  * Created by chris on 5/16/16.
  * source of values: https://en.bitcoin.it/wiki/Base58Check_encoding
  */
trait Base58 extends BitcoinSLogger {

  val base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val base58Pairs = base58Characters.zipWithIndex.toMap

  /**
    * Verifies a given base58 string against its checksum (last 4 decoded bytes)
    * @param input base58 string
    * @return decoded bytes excluding the checksum
    */
  def decodeCheck(input: String) : Try[Seq[Byte]] = {

    val decoded : Seq[Byte] = decode(input)
    if (decoded.length < 4) Failure(new IllegalArgumentException("Invalid input"))
    else {
      val splitSeqs = decoded.splitAt(decoded.length - 4)
      val data : Seq[Byte] = splitSeqs._1
      val checksum : Seq[Byte] = splitSeqs._2
      val actualChecksum : Seq[Byte] = CryptoUtil.doubleSHA256(data).slice(0, 4)
      if (checksum == actualChecksum)
      Success(data)
      else Failure(new IllegalArgumentException("checksums don't validate"))
    }
  }


  /**
    * Takes in sequence of bytes and returns base58 bitcoin string
    * @param bytes sequence of bytes to be encoded into base58
    * @return base58 String
    */

  def encode(bytes : Seq[Byte]) : String = {
    @tailrec
    def loop(current : BigInt, str : String) : String = current match {
      case a if current == BigInt(0) =>
        if (bytes.head == 0.toByte) '1' + str.reverse else str.reverse
      case _ : BigInt =>
        val quotient : BigInt = current / BigInt(58L)
        val remainder : BigInt  = current.mod(58L)
        val char = base58Characters.charAt(remainder.toInt).toString
        val accum =  str + char
        loop(quotient, accum)
    }
    if (bytes.isEmpty) ""
    else {
      val big : BigInt = BigInt(1, bytes.toArray)
      loop(big, "")
    }
  }


  /**
    * Takes in base58 string and returns sequence of bytes
    * https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
    * @param input base58 string to be decoded into a sequence of bytes
    * @return decoded sequence of bytes
    */
  def decode(input: String) : Seq[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0:Byte).toArray
    val trim  = input.dropWhile(_ == '1').toList
    val decoded = trim.foldLeft(BigInt(0))((a,b) =>a.*(BigInt(58L)).+(BigInt(base58Pairs(b))))
    if (trim.isEmpty) zeroes else zeroes ++ decoded.toByteArray.dropWhile(_ == 0)
  }

}

object Base58 extends Base58



