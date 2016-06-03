package org.bitcoins.core.protocol
import org.bitcoins.core.config.{RegTest, TestNet3, MainNet}
import org.bitcoins.core.util.{Factory, BitcoinSUtil}
import org.bitcoins.core.config.{RegTest, TestNet3, MainNet}
import org.bitcoins.core.util.{CryptoUtil, Base58, Factory}
import scala.util.{Failure, Success, Try}

sealed abstract class Address(val value : String)

sealed case class BitcoinAddress(override val value: String) extends Address(value ) {
  require(BitcoinAddress.validate(value), "Bitcoin address was invalid " + value)
}

sealed case class AssetAddress(override val value : String) extends Address(value) {
  require(AssetAddress.validate(value), "The provided asset was invalid: " + value)
}

object BitcoinAddress {
  def validate(bitcoinAddress: String): Boolean = {
    val illegalChars = List('O', 'I', 'l', '0')
    bitcoinAddress.length >= 26 && bitcoinAddress.length <= 35 &&
      (p2pkh(bitcoinAddress) || p2shAddress(bitcoinAddress)) &&
      bitcoinAddress.filter(c => illegalChars.contains(c)).size == 0
  }

  /**
   * Converts a bitcoin address to an asset address
    *
    * @param address
   * @return
   */
  def convertToAssetAddress(address : BitcoinAddress) : AssetAddress = {
    val underlying : String  = address.value
    val decodedBase58 : Seq[Byte] = Base58.decode(underlying)
    require (
      decodedBase58.size == 25
    )
    val decodedWithNameSpaceByte = Seq(0x13.toByte) ++ decodedBase58
    val split = decodedWithNameSpaceByte.splitAt(decodedWithNameSpaceByte.length - 4)
    val data = split._1
    val newCheckSum = CryptoUtil.doubleSHA256(data).bytes.slice(0,4)
    val constructedAssetAddress = data ++ newCheckSum
    val encodedAssetAddress = Base58.encode(constructedAssetAddress)
    AssetAddress(encodedAssetAddress)
  }

  /**
   * Checks if a address is a valid p2sh address
    *
    * @param address
   * @return
   */
  def p2shAddress(address : String) : Boolean = {
    val decodeCheckP2SH : Try[Seq[Byte]] = Base58.decodeCheck(address)
    decodeCheckP2SH match {
      case Success(bytes) =>
        val firstByte = bytes.head
        ((firstByte == MainNet.p2shNetworkByte || firstByte == TestNet3.p2shNetworkByte ||
          RegTest.p2shNetworkByte == firstByte)
          && bytes.size == 21)
      case Failure(exception) => false
    }
  }

  /**
   * Checks if a address is a valid p2sh address
    *
    * @param address
   * @return
   */
  def p2shAddress(address : BitcoinAddress) : Boolean = p2shAddress(address.value)

  /**
   * Checks if an address is a valid p2pkh address
    *
    * @param address
   * @return
   */
  def p2pkh(address : String) : Boolean = {
    val decodeCheckP2PKH : Try[Seq[Byte]] = Base58.decodeCheck(address)
    decodeCheckP2PKH match {
      case Success(bytes) =>
        val firstByte = bytes.head
        (firstByte == MainNet.p2pkhNetworkByte || firstByte == TestNet3.p2pkhNetworkByte ||
          firstByte == RegTest.p2pkhNetworkByte) && bytes.size == 21
      case Failure(exception) => false
    }
  }

  /**
   * Checks if an address is a valid p2pkh address
    *
    * @param address
   * @return
   */
  def p2pkh(address : BitcoinAddress) : Boolean = p2pkh(address.value)
}

object AssetAddress {
  def validate(assetAddress : String) : Boolean = {
    //asset addresses must have the one byte namespace equivalent to 19
    //which ends up being 'a' in the ascii character set.
    //bytes size becomes 22
    val decodeCheckAssetAddress : Try[Seq[Byte]] = Base58.decodeCheck(assetAddress)
    decodeCheckAssetAddress match {
      case Success(bytes) => bytes.size == 22  && bytes.head == 0x13
      case Failure(_) => false
    }
  }

  /**
   * Converts an asset address into a bitcoin address
    *
    * @param assetAddress
   * @return
   */
  def convertToBitcoinAddress(assetAddress : AssetAddress) : BitcoinAddress = {
    val underlying : String = assetAddress.value
    val decodedAsset = Base58.decode(underlying)
    require {
      decodedAsset.size == 26
    }
    val data = decodedAsset.slice(0, decodedAsset.length - 4)
    val dataDroppedNameSpace = data.drop(1)
    val checkSum = CryptoUtil.doubleSHA256(dataDroppedNameSpace).bytes.slice(0,4)
    val value = Base58.encode(dataDroppedNameSpace ++ checkSum)
    BitcoinAddress(value)
  }
}

object Address extends Factory[Address] {
  /**
    * Factory method for creating addresses
    * Takes in a string to check if it is an address
    * if it is it creates the address
    * if not it throws a runtime exception
    *
    * @param str
    * @return
    */
  def factory(str : String) : Address = {
    if (AssetAddress.validate(str)) AssetAddress(str)
    else if (BitcoinAddress.validate(str)) BitcoinAddress(str)
    else throw new RuntimeException("The address that you passed in is invalid")
  }


  def fromBytes(bytes : Seq[Byte]) : Address = factory(Base58.encode(bytes))

  override def fromHex(hex : String) : Address = throw new RuntimeException("We cannot create a bitcoin address from hex - bitcoin addresses are base 58 encoded")

  override def apply(str : String) : Address = factory(str)
}