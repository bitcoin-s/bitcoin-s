package org.bitcoins.core.crypto

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.util.{Base58, BytesUtil}
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

object ECPrivateKeyUtil {

  /**
    * Converts a [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]] to
    * [[https://en.bitcoin.it/wiki/Wallet_import_format WIF]]
    */
  def toWIF(privKey: ECPrivateKey, network: NetworkParameters): String = {
    val networkByte = network.privateKey
    //append 1 byte to the end of the priv key byte representation if we need a compressed pub key
    val fullBytes =
      if (privKey.isCompressed) networkByte ++ (privKey.bytes ++ ByteVector(1))
      else networkByte ++ privKey.bytes
    val hash = CryptoUtil.doubleSHA256(fullBytes)
    val checksum = hash.bytes.take(4)
    val encodedPrivKey = fullBytes ++ checksum
    Base58.encode(encodedPrivKey)
  }

  /**
    * Takes in a base58 string and converts it into a private key.
    * Private keys starting with 'K', 'L', or 'c' correspond to compressed public keys.
    * https://en.bitcoin.it/wiki/Wallet_import_format
    *
    * @param WIF Wallet Import Format. Encoded in Base58
    * @return
    */
  def fromWIFToPrivateKey(WIF: String): ECPrivateKey = {
    val isCompressed = ECPrivateKeyUtil.isCompressed(WIF)
    val privateKeyBytes = trimFunction(WIF)
    ECPrivateKey.fromBytes(privateKeyBytes, isCompressed)
  }

  /**
    * Takes in WIF private key as a sequence of bytes and determines if it corresponds to a compressed public key.
    * If the private key corresponds to a compressed public key, the last byte should be 0x01, and
    * the WIF string will have started with K or L instead of 5 (or c instead of 9 on testnet).
    *
    * @param bytes private key in bytes
    * @return
    */
  def isCompressed(bytes: ByteVector): Boolean = {
    val validCompressedBytes: Seq[ByteVector] = Networks.secretKeyBytes
    val validCompressedBytesInHex: Seq[String] =
      validCompressedBytes.map(b => BytesUtil.encodeHex(b))
    val firstByteHex = BytesUtil.encodeHex(bytes.head)
    if (validCompressedBytesInHex.contains(firstByteHex))
      bytes(bytes.length - 5) == 0x01.toByte
    else false
  }

  def isCompressed(WIF: String): Boolean = {
    val bytes = Base58.decode(WIF)
    isCompressed(bytes)
  }

  /**
    * When decoding a WIF private key, we drop the first byte (network byte), and the last 4 bytes (checksum).
    * If the private key corresponds to a compressed public key, we drop the last byte again.
    * https://en.bitcoin.it/wiki/Wallet_import_format
    * @param WIF Wallet Import Format. Encoded in Base58
    * @return
    */
  private def trimFunction(WIF: String): ByteVector = {
    val bytesChecked = Base58.decodeCheck(WIF)

    //see https://en.bitcoin.it/wiki/List_of_address_prefixes
    //for where '5' and '9' come from
    bytesChecked match {
      case Success(bytes) if uncompressedKeyPrefixes.contains(WIF.headOption) =>
        bytes.tail
      case Success(bytes) if isCompressed(WIF) => bytes.tail.dropRight(1)
      case Success(bytes)                      => bytes.tail
      case Failure(exception)                  => throw exception
    }
  }

  /** The Base58 prefixes that represent compressed private keys */
  def compressedKeyPrefixes = Seq(Some('K'), Some('L'), Some('c'))

  /** The Base58 prefixes that represent uncompressed private keys */
  def uncompressedKeyPrefixes = Seq(Some('5'), Some('9'))

  /** Returns the [[org.bitcoins.core.config.NetworkParameters NetworkParameters]] from a serialized WIF key */
  def parseNetworkFromWIF(wif: String): Try[NetworkParameters] = {
    val decoded = Base58.decodeCheck(wif)
    decoded match {
      case Success(bytes) =>
        val networkOpt =
          Networks.secretKeyBytes.find(b => bytes.startsWith(b))
        networkOpt match {
          case Some(network) =>
            Success(Networks.bytesToNetwork(network))
          case None =>
            Failure(
              new IllegalArgumentException(
                "Failed to match network bytes for WIF"))
        }
      case Failure(exn) => Failure(exn)
    }
  }
}
