package org.bitcoins.core.util

import org.bitcoins.core.config.{MainNet, TestNet3}
import org.bitcoins.core.crypto.{ECPrivateKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.blockchain._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 5/16/16.
  * source of values: https://en.bitcoin.it/wiki/Base58Check_encoding
  */
trait Base58 extends BitcoinSLogger {

  val base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val base58Pairs = base58Characters.zipWithIndex.toMap

  /**
    * Verifies a given base58 string against its checksum (last 4 decoded bytes)
    *
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
      val actualChecksum : Seq[Byte] = CryptoUtil.doubleSHA256(data).bytes.slice(0, 4)
      if (checksum == actualChecksum) Success(data)
      else Failure(new IllegalArgumentException("checksums don't validate"))
    }
  }


  /**
    * Takes in sequence of bytes and returns base58 bitcoin string
    *
    * @param bytes sequence of bytes to be encoded into base58
    * @return base58 String
    */
  //TODO: Create Base58 Type
  def encode(bytes : Seq[Byte]) : String = {
    val ones : String = bytes.takeWhile(_ == 0).map(_ => '1').mkString
    @tailrec
    def loop(current : BigInt, str : String) : String = current match {
      case a if current == BigInt(0) =>
        ones + str.reverse
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

  def encode(hex : String) : String = {
    val bytes = BitcoinSUtil.decodeHex(hex)
    encode(bytes)
  }

  def encode(byte : Byte) : String = encode(Seq(byte))

  /**
    * Encodes a Base58 address from a hash
    *
    * @param hash The result of Sha256(RipeMD-160(public key))
    * @param addressType string. Either "pubkey" or "script"
    * @param isTestNet boolean
    * @return
    */
  def encodePubKeyHashToBase58Address(hash: Sha256Hash160Digest,
                                  addressType : String,
                                  isTestNet : Boolean) : String = {
/*    val versionByte : Byte = {
      require(addressType != "pubkey" || addressType != "script",
        throw new IllegalArgumentException("Address must be of type 'pubkey' or 'script'."))
      if (!isTestNet) {
        if (addressType == "pubkey") MainNet.p2pkhNetworkByte
        else MainNet.p2shNetworkByte
      }
      else if (isTestNet) {
        if (addressType == "pubkey") TestNet3.p2pkhNetworkByte
        else TestNet3.p2shNetworkByte
      }
      else throw new IllegalArgumentException("Something broke -- Check your parameters. There should be a " +
        "Sha256RipeMD160 hash, addressType('pubkey' or 'script'), and testnet boolean (true or false).")
    }*/

    def parseVersionByte(addressType : String, isTestnet : Boolean) : Byte = isTestnet match {
      case true => if (addressType == "pubkey") TestNet3.p2pkhNetworkByte else TestNet3.p2shNetworkByte
      case false => if (addressType == "pubkey") MainNet.p2pkhNetworkByte else MainNet.p2shNetworkByte
    }
    val versionByte : Byte = parseVersionByte(addressType, isTestNet)
    val bytes : Seq[Byte] = Seq(versionByte) ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    encode(bytes ++ checksum)
  }

  /**
    * Encodes a private key into Wallet Import Format (WIF)
    * https://en.bitcoin.it/wiki/Wallet_import_format
    *
    * @param privateKey
    * @param isCompressed
    * @param isTestNet
    * @return
    */
  //TODO: Create WIF PrivateKey Type
  def encodePrivateKeyToWIF(privateKey : ECPrivateKey,
                            isCompressed : Boolean,
                            isTestNet : Boolean) : String = {
    val versionByte : Byte = isTestNet match {
      case true => TestNet3.privateKey
      case false => MainNet.privateKey
    }
    val compressedByte : Option[Byte] = isCompressed match {
      case true => Some(0x01.toByte)
      case false => None
    }
    val bytes : Seq[Byte] = Seq(versionByte.toByte) ++ BitcoinSUtil.decodeHex(privateKey.hex) ++ compressedByte
    val checksum =  CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    encode(bytes ++ checksum)
  }

  /**
    * Takes in base58 string and returns sequence of bytes
    * https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
    *
    * @param input base58 string to be decoded into a sequence of bytes
    * @return decoded sequence of bytes
    */
  def decode(input: String) : Seq[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0:Byte).toArray
    val trim  = input.dropWhile(_ == '1').toList
    val decoded = trim.foldLeft(BigInt(0))((a,b) =>
      a.*(BigInt(58L)).+(BigInt(base58Pairs(b))))
    if (trim.isEmpty) zeroes else zeroes ++ decoded.toByteArray.dropWhile(_ == 0)
  }

  def isValid(base58 : String) : Boolean = {
    if (base58.isEmpty) false
    else {
      val decoded = decode(base58)
      val firstByte = decoded.head
      val validAddressPreFixBytes: Seq[Byte] =
        MainNetChainParams.base58Prefixes(PubKeyAddress) ++ MainNetChainParams.base58Prefixes(ScriptAddress) ++
          TestNetChainParams.base58Prefixes(PubKeyAddress) ++ TestNetChainParams.base58Prefixes(ScriptAddress)
      val validSecretKeyPreFixBytes : Seq[Byte] =
        MainNetChainParams.base58Prefixes(SecretKey) ++ TestNetChainParams.base58Prefixes(SecretKey)
      val compressedPubKey = List('K', 'L', 'c').contains(base58.head)
      def checkCompressedPubKeyValidity : Boolean = {
        val compressedByte = decoded(decoded.length - 5)
        compressedByte == 0x01.toByte
      }
      try {
        if (base58.contains(List('0', 'O', 'l', 'I'))) false
        else if (compressedPubKey) checkCompressedPubKeyValidity
        else if (validAddressPreFixBytes.contains(firstByte)) base58.length >= 26 && base58.length <= 35
        else if (validSecretKeyPreFixBytes.contains(firstByte)) {
          val byteSize = ECPrivateKey.fromBase58ToPrivateKey(base58).bytes.size
          byteSize == 32
        }
        else false
      }
      catch {
        case error : IllegalArgumentException => false
      }
    }
  }

/*    def isValid(base58 : String) : Boolean = {
    val firstByte : Seq[Byte]= if (base58.isEmpty) List() else Seq(decode(base58).head)
    val length = base58.length
    val validFirstByteInHex = List("00", "05", "80", "6f", "c4", "ef")
    val invalidChars = List('0','O','l','I')
    val firstByteInHex = BitcoinSUtil.encodeHex(firstByte)
    if (!validFirstByteInHex.contains(firstByteInHex)) false
    else if (length < 25 || length > 36) false
    else if (base58.contains(invalidChars)) false
    else true
  }*/
}

object Base58 extends Base58



