package org.bitcoins.core.protocol
import org.bitcoins.core.config._
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.script.{P2SHScriptPubKey, ScriptPubKey}
import org.bitcoins.core.util.{Base58, CryptoUtil, Factory}
import scala.util.{Failure, Success, Try}

sealed trait Address {

  /** The network that this address is valid for */
  def networkParameters: NetworkParameters

  /** The base58 string representation of this address */
  def value : String

  /** Every address is derived from a [[Sha256Hash160Digest]] in a [[TransactionOutput]] */
  def hash: Sha256Hash160Digest
}

sealed trait BitcoinAddress extends Address

sealed trait P2PKHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value : String = {
    val versionByte = networkParameters.p2pkhNetworkByte
    val bytes = Seq(versionByte) ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

}

object P2PKHAddress {
  private case class P2PKHAddressImpl(hash: Sha256Hash160Digest,
                                      networkParameters: NetworkParameters) extends P2PKHAddress {
    require(isP2PKHAddress(value), "Bitcoin address was invalid " + value)
  }

  /**
    * Encodes a pubkey hash to a base 58 address on the corresponding network
    *
    * @param hash the result of Sha256(RipeMD160(pubkey))
    * @param network the network on which this address is being generated for
    * @return
    */
  def encodePubKeyHashToAddress(hash: Sha256Hash160Digest, network: NetworkParameters): P2PKHAddress = P2PKHAddressImpl(hash,network)

  def apply(hash: Sha256Hash160Digest, networkParameters: NetworkParameters): P2PKHAddress = encodePubKeyHashToAddress(hash,networkParameters)

  def apply(pubKey: ECPublicKey, networkParameters: NetworkParameters): P2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash,networkParameters)
  }

  /**
    * Checks if an address is a valid p2pkh address
    *
    * @param address
    * @return
    */
  def isP2PKHAddress(address : String) : Boolean = {
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
  def isP2PKHAddress(address : BitcoinAddress) : Boolean = isP2PKHAddress(address.value)

}

sealed trait P2SHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value : String = {
    val versionByte = networkParameters.p2shNetworkByte
    val bytes = Seq(versionByte) ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }
}

/**
  * [[P2SHAddress]] companion object
  */
object P2SHAddress {
  private case class P2SHAddressImpl(hash: Sha256Hash160Digest,
                                     networkParameters: NetworkParameters) extends P2SHAddress {
    require(isP2SHAddress(value), "Bitcoin address was invalid " + value)
  }

  /**
    * Takes in an arbitrary [[ScriptPubKey]] and [[NetworkParameters]] and creates a [[P2SHAddress]]
    *
    * @param scriptPubKey the script which will need to provided as the redeem script
    * @param network the network which this address is valid for
    * @return the [[P2SHAddress]]
    */
  def encodeScriptPubKeyToAddress(scriptPubKey: ScriptPubKey, network: NetworkParameters): P2SHAddress = {
    val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
    P2SHAddress(p2shScriptPubKey,network)
  }


  /**
    * Creates a [[P2SHScriptPubKey]] from the given [[ScriptPubKey]], then creates an address from that [[P2SHScriptPubKey]]
    * @param scriptPubKey
    * @param network
    * @return
    */
  def apply(scriptPubKey: ScriptPubKey,network: NetworkParameters): P2SHAddress = encodeScriptPubKeyToAddress(scriptPubKey,network)


  def apply(p2shScriptPubKey: P2SHScriptPubKey, network: NetworkParameters): P2SHAddress = P2SHAddress(p2shScriptPubKey.scriptHash,network)


  def apply(hash: Sha256Hash160Digest, network: NetworkParameters): P2SHAddress = P2SHAddressImpl(hash, network)

  /**
    * Checks if a address is a valid p2sh address
    * @param address
    * @return
    */
  def isP2SHAddress(address : String) : Boolean = {
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
  def isP2SHAddress(address : BitcoinAddress) : Boolean = isP2SHAddress(address.value)

}

object BitcoinAddress {
  /** Checks if the given base58 bitcoin address is a valid address */
  def validate(bitcoinAddress: String): Boolean = {
    val decodeChecked = Base58.decodeCheck(bitcoinAddress)
    decodeChecked.isSuccess
  }


  /** Creates a [[BitcoinAddress]] from the given base58 string value */
  def apply(value: String): BitcoinAddress = {
    val decodeChecked = Base58.decodeCheck(value)
    decodeChecked match {
      case Success(bytes) =>
        val network = matchNetwork(bytes.head)
        if (P2PKHAddress.isP2PKHAddress(value)) {
          P2PKHAddress(Sha256Hash160Digest(bytes.tail),network)
        }
        else if (P2SHAddress.isP2SHAddress(value)) {
          P2SHAddress(Sha256Hash160Digest(bytes.tail), network)
        } else throw new IllegalArgumentException("The address was not a p2pkh or p2sh address, got: " + value)
      case Failure(exception) =>
        throw exception
    }
  }

  /** Helper function for helping matching an address to a network byte */
  private def matchNetwork(byte: Byte): NetworkParameters = byte match {
    case _ if Seq(MainNet.p2pkhNetworkByte,MainNet.p2shNetworkByte).contains(byte) => MainNet
    case _ if Seq(TestNet3.p2pkhNetworkByte, TestNet3.p2shNetworkByte).contains(byte) => TestNet3
    case _ if Seq(RegTest.p2pkhNetworkByte,RegTest.p2shNetworkByte).contains(byte) => RegTest
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
    if (BitcoinAddress.validate(str)) BitcoinAddress(str)
    else throw new RuntimeException("The address that you passed in is invalid")
  }

  def fromBytes(bytes : Seq[Byte]) : Address = factory(Base58.encode(bytes))

  override def fromHex(hex : String) : Address = throw new RuntimeException("We cannot create a bitcoin address from hex - bitcoin addresses are base 58 encoded")

  override def apply(str : String) : Address = factory(str)
}