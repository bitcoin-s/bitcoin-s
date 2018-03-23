package org.bitcoins.core.util

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.blockchain._

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 5/16/16.
 * source of values: https://en.bitcoin.it/wiki/Base58Check_encoding
 */
sealed abstract class Base58 {
  import Base58Type._
  val base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val base58Pairs = base58Characters.zipWithIndex.toMap
  private val logger = BitcoinSLogger.logger
  /** Verifies a given [[Base58Type]] string against its checksum (last 4 decoded bytes). */
  def decodeCheck(input: String): Try[Seq[Byte]] = {
    val decoded: Seq[Byte] = decode(input)
    if (decoded.length < 4) Failure(new IllegalArgumentException("Invalid input"))
    else {
      val splitSeqs = decoded.splitAt(decoded.length - 4)
      val data: Seq[Byte] = splitSeqs._1
      val checksum: Seq[Byte] = splitSeqs._2
      val actualChecksum: Seq[Byte] = CryptoUtil.doubleSHA256(data).bytes.take(4)
      if (checksum == actualChecksum) Success(data)
      else Failure(new IllegalArgumentException("checksums don't validate"))
    }
  }

  /** Encodes a sequence of bytes to a [[Base58Type]] string. */
  def encode(bytes: Seq[Byte]): String = {
    val ones: String = bytes.takeWhile(_ == 0).map(_ => '1').mkString
    @tailrec
    def loop(current: BigInt, str: String): String = current match {
      case a if current == BigInt(0) =>
        ones + str.reverse
      case _: BigInt =>
        val quotient: BigInt = current / BigInt(58L)
        val remainder: BigInt = current.mod(58L)
        val char = base58Characters.charAt(remainder.toInt).toString
        val accum = str + char
        loop(quotient, accum)
    }
    if (bytes.isEmpty) ""
    else {
      val big: BigInt = BigInt(1, bytes.toArray)
      loop(big, "")
    }
  }

  /** Encodes a hex string to its [[Base58Type]] representation. */
  def encode(hex: String): String = {
    val bytes = BitcoinSUtil.decodeHex(hex)
    encode(bytes)
  }

  /** Encodes a [[Byte]] to its [[Base58Type]] representation. */
  def encode(byte: Byte): String = encode(Seq(byte))

  /**
   * Takes in [[Base58Type]] string and returns sequence of [[Byte]]s
   * https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala.
   */
  def decode(input: String): Seq[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0: Byte).toArray
    val trim = input.dropWhile(_ == '1').toList
    val decoded = trim.foldLeft(BigInt(0))((a, b) =>
      a.*(BigInt(58L)).+(BigInt(base58Pairs(b))))
    if (trim.isEmpty) zeroes else zeroes ++ decoded.toByteArray.dropWhile(_ == 0)
  }

  /** Determines if a string is a valid [[Base58Type]] string. */
  def isValid(base58: String): Boolean = validityChecks(base58) match {
    case Success(bool)      => bool
    case Failure(exception) => false
  }

  /**
   * Checks a private key that begins with a symbol corresponding that private key to a compressed public key ('K', 'L', 'c').
   * In a Base58-encoded private key corresponding to a compressed public key, the 5th-to-last byte should be 0x01.
   */
  private def checkCompressedPubKeyValidity(base58: String): Boolean = {
    val decoded = Base58.decode(base58)
    val compressedByte = decoded(decoded.length - 5)
    compressedByte == 0x01.toByte
  }

  /**
   * Checks if the string begins with an Address prefix byte/character.
   * ('1', '3', 'm', 'n', '2')
   */
  private def isValidAddressPreFixByte(byte: Byte): Boolean = {
    val validAddressPreFixBytes: Seq[Byte] =
      MainNetChainParams.base58Prefixes(PubKeyAddress) ++ MainNetChainParams.base58Prefixes(ScriptAddress) ++
        TestNetChainParams.base58Prefixes(PubKeyAddress) ++ TestNetChainParams.base58Prefixes(ScriptAddress)
    validAddressPreFixBytes.contains(byte)
  }

  /**
   * Checks if the string begins with a private key prefix byte/character.
   * ('5', '9', 'c')
   */
  private def isValidSecretKeyPreFixByte(byte: Byte): Boolean = {
    val validSecretKeyPreFixBytes: Seq[Byte] =
      MainNetChainParams.base58Prefixes(SecretKey) ++ TestNetChainParams.base58Prefixes(SecretKey)
    validSecretKeyPreFixBytes.contains(byte)
  }

  /**
   * Checks the validity of a [[Base58Type]] string. A [[Base58Type]] string must not contain ('0', 'O', 'l', 'I').
   * If the string is an address: it must have a valid address prefix byte and  must be between 26-35 characters in length.
   * If the string is a private key: it must have a valid private key prefix byte and must have a byte size of 32.
   * If the string is a private key corresponding to a compressed public key, the 5th-to-last byte must be 0x01.
   */
  private def validityChecks(base58: String): Try[Boolean] = Try {
    val decoded = decode(base58)
    val firstByte = decoded.head
    val compressedPubKey = List('K', 'L', 'c').contains(base58.head)
    if (base58.contains(List('0', 'O', 'l', 'I'))) false
    else if (compressedPubKey) checkCompressedPubKeyValidity(base58)
    else if (isValidAddressPreFixByte(firstByte)) base58.length >= 26 && base58.length <= 35
    else if (isValidSecretKeyPreFixByte(firstByte)) {
      val byteSize = ECPrivateKey.fromWIFToPrivateKey(base58).bytes.size
      byteSize == 32
    } else false
  }
}

object Base58 extends Base58